import cats.implicits._
import cats.data.EitherT
import cats.effect.{ExitCode, IO, IOApp}
import Ops._
import org.http4s.Uri
import org.slf4j.LoggerFactory

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val logger = LoggerFactory.getLogger(this.getClass)

    val params = for {
      usersListLocation <- tryFindKey("NAMES_LOCATION", Some("/screen_names.txt"))
      screenNames       <- readUsersList(usersListLocation)
      chHost            <- readEnv("CH_HOST")
      chUser            <- tryFindKey("CH_USER", Some("default"))
      chPass            <- tryFindKey("CH_PASS", Some(""))
      chTable           <- readEnv("CH_TABLE")
      chHostUri         <- EitherT(IO(Uri.fromString(chHost).leftMap(_.getMessage)))
      consumerKey       <- tryFindKey("TW_CONSUMER_KEY")
      consumerSecret    <- tryFindKey("TW_CONSUMER_SECRET")
      accessToken       <- tryFindKey("TW_ACCESS_TOKEN")
      accessSecret      <- tryFindKey("TW_ACCESS_SECRET")
      tgChatId          <- readEnv("TG_CHAT")
      tgBotToken        <- tryFindKey("TG_TOKEN")
    } yield
      (
        Twitter(consumerKey, consumerSecret, accessToken, accessSecret),
        screenNames,
        Clickhouse(chHostUri, chUser, chPass, chTable),
        Telegram[IO](tgBotToken, tgChatId)
      )

    val program = for {
      ps <- params
      stream <- new TweetStream[IO](ps._1, ps._2, ps._3, ps._4).run
                 .as(ExitCode.Success)
                 .attemptT
                 .leftMap(_.getMessage)
    } yield stream

    program.valueOrF(err => IO(logger.error(err)) >> IO(ExitCode.Error))
  }
}
