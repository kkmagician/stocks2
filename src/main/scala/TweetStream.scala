import cats.implicits._
import org.http4s._
import org.http4s.client.blaze._
import org.http4s.client.{Client, oauth1}
import org.http4s.implicits._
import org.http4s.circe._
import cats.effect._
import fs2.Stream
import fs2.text.{lines, utf8Encode}
import io.circe.Json
import jawnfs2._
import org.typelevel.jawn.Facade
import io.circe.optics.JsonPath._
import monocle.Traversal
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext.global

class TweetStream[F[_]: ConcurrentEffect: ContextShift](
  tw: Twitter,
  screenNames: List[String],
  ch: Clickhouse,
  tg: Telegram[F]
) {
  implicit val f: Facade[Json] = new io.circe.jawn.CirceSupportParser(None, false).facade
  private val logger = LoggerFactory.getLogger(this.getClass)

  def sign(req: Request[F]): F[Request[F]] = {
    val consumer = oauth1.Consumer(tw.consumerKey, tw.consumerSecret)
    val token = oauth1.Token(tw.accessToken, tw.accessSecret)
    oauth1.signRequest(
      req,
      consumer,
      callback = None,
      verifier = None,
      token = Some(token)
    )
  }

  def findUserIds(screenNames: Seq[String]): F[Request[F]] =
    sign(
      Request[F](
        Method.POST,
        uri"https://api.twitter.com/1.1/users/lookup.json"
          .withQueryParam("screen_name", screenNames)
      )
    )

  val userIdOptic: Traversal[Json, String] = root.each.id_str.string

  def getUserIds(
    screenNames: Seq[String]
  )(client: Client[F]): F[Vector[String]] =
    client
      .expectOr[Json](findUserIds(screenNames))(_.as[String].map(err => new Throwable(err)))
      .map(userIdOptic.getAll)
      .map(_.toVector)

  def createStreamRequest(userIds: Seq[String]): Request[F] = Request[F](
    Method.GET,
    uri"https://stream.twitter.com/1.1/statuses/filter.json".withMultiValueQueryParams(
      Map("follow" -> userIds, "tweet_mode" -> List("extended"))
    )
  )

  def createTelegramRequest(text: String, tg: Telegram[F]): Request[F] = {
    val msg = UrlForm(
      "chat_id" -> tg.chatId,
      "text" -> text,
      "parse_mode" -> "MarkdownV2",
      "disable_web_page_preview" -> "true"
    )

    Request[F](
      Method.POST,
      uri"https://api.telegram.org" / ("bot" ++ tg.botToken) / "sendMessage"
    ).withEntity(msg)
  }

  def sendTgMessage(text: String, tg: Telegram[F], client: Client[F]): F[Json] =
    client.expectOr[Json](createTelegramRequest(text, tg))(
      r => r.as[Json].map(e => new Throwable(e.noSpaces))
    )

  def makeTweetDataStream(
    request: Request[F],
    client: Client[F]
  ): Stream[F, TweetData] =
    client
      .stream(request)
      .flatMap(_.body.chunks.parseJsonStream)
      .mapFilter(Tweet.apply)
      .filter(tw => screenNames.contains(tw.userName))
      .flatMap(tw => Stream.eval(TweetData(tw, client)))
      .filter(_.symbols.nonEmpty)

  def jsonStream(
    blocker: Blocker
  )(implicit eff: Concurrent[F], timer: Timer[F]): Stream[F, TweetData] =
    for {
      client   <- BlazeClientBuilder(global).stream
      tgSender <- Stream.eval(eff.pure(tg.sendTweetMessage(client) _))
      userIds  <- Stream.eval(getUserIds(screenNames)(client))
      sr       <- Stream.eval(sign(createStreamRequest(userIds)))
      res      <- makeTweetDataStream(sr, client)
      _ = logger.info(res.toString)
      _ <- Stream.eval(tgSender(res).attempt.recoverWith({ case _ => tgSender(res).attempt }))
      _ <- Stream.eval(res.writeToCh(ch, client))
    } yield res

  def run(implicit timer: Timer[F]): F[Unit] =
    Stream
      .resource(Blocker[F])
      .flatMap { blocker =>
        jsonStream(blocker)
      }
      .map(_.toString)
      .through(lines)
      .through(utf8Encode)
      .compile
      .drain
}
