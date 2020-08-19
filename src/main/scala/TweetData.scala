import cats.implicits._
import cats.effect.{ConcurrentEffect, ContextShift}
import io.circe.Json
import io.circe.syntax._
import org.http4s.Request
import org.http4s._
import org.http4s.client.Client
import org.http4s.headers._
import Ops.StringOps

case class TweetData(
  id: Long,
  createdAt: Long,
  truncated: Boolean,
  text: String,
  userId: Long,
  userName: String,
  replyTo: String,
  retweetedFrom: String,
  symbols: List[Symbol],
  media: List[String],
  hashtags: List[String]
) {
  def toClickhouseJson: Json =
    Map(
      "id" -> id.asJson,
      "createdAt" -> createdAt.asJson,
      "truncated" -> truncated.asJson,
      "text" -> text.asJson,
      "userId" -> userId.asJson,
      "userName" -> userName.asJson,
      "replyTo" -> replyTo.asJson,
      "retweetedFrom" -> retweetedFrom.asJson,
      "symbols.ticker" -> symbols.map(_.ticker).asJson,
      "symbols.quoteType" -> symbols.map(_.quoteType).asJson,
      "symbols.marketCap" -> symbols.map(_.marketCap).asJson,
      "symbols.marketState" -> symbols.map(_.marketState).asJson,
      "symbols.currentPrice" -> symbols.map(_.currentPrice).asJson,
      "symbols.previousClose" -> symbols.map(_.previousClose).asJson,
      "symbols.marketStatePrice" -> symbols.map(_.marketStatePrice).asJson,
      "symbols.yfRecommendationKey" -> symbols.map(_.yfRecommendationKey).asJson,
      "symbols.open" -> symbols.map(_.open).asJson,
      "symbols.dayLow" -> symbols.map(_.dayLow).asJson,
      "symbols.dayHigh" -> symbols.map(_.dayHigh).asJson,
      "symbols.fiftyTwoWeekLow" -> symbols.map(_.fiftyTwoWeekLow).asJson,
      "symbols.fiftyTwoWeekHigh" -> symbols.map(_.fiftyTwoWeekHigh).asJson,
      "symbols.twoHundredDayAverage" -> symbols.map(_.twoHundredDayAverage).asJson,
      "symbols.fiftyDayAverage" -> symbols.map(_.fiftyDayAverage).asJson,
      "media" -> media.asJson,
      "hashtags" -> hashtags.asJson
    ).asJson

  def makeChRequest[F[_]: ConcurrentEffect: ContextShift](ch: Clickhouse): Request[F] =
    Request[F](Method.POST, ch.host)
      .withHeaders(Authorization(BasicCredentials(ch.user, ch.password)))
      .withEntity(s"insert into ${ch.table} format JSONEachRow " ++ this.toClickhouseJson.noSpaces)

  def writeToCh[F[_]: ConcurrentEffect: ContextShift](
    ch: Clickhouse,
    client: Client[F]
  ): F[String] =
    client.expectOr[String](makeChRequest(ch))(_.as[String].map(err => new Throwable(err)))

  def makeTwitterLink: String = s"https://twitter.com/$userName/status/$id".escapeMarkdown

  def toTelegramMessage: String =
    (symbols.map(_.toTelegramString) ++ List(
      s"#$userName: $text".escapeMarkdown,
      s"[link](${this.makeTwitterLink})"
    )).mkString("\n")
}

object TweetData {
  private def filteredSymbol(s: Option[Symbol]): Option[Symbol] = s match {
    case Some(s) if s.marketCap > 20000000000L || s.quoteType == "FUTURE" => Some(s)
    case _                                                                => None
  }

  private val tickerCleaned = (ticker: String) =>
    ticker.replace('_', '=').toUpperCase.replace("SPX", "SPY")

  def apply[F[_]: ConcurrentEffect: ContextShift](
    tweet: Tweet,
    client: Client[F],
  ): F[TweetData] = {
    val add = if (tweet.text.toLowerCase.contains("market")) List("SPY") else List()
    (add ++ tweet.symbols)
      .map(tickerCleaned)
      .distinct
      .map(Symbol(_, client))
      .sequence
      .map(_.mapFilter(filteredSymbol))
      .map(tweet.toTweetData)
  }
}
