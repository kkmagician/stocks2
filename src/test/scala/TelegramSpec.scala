import java.util.concurrent.Executors

import cats.implicits._
import cats.effect.{Blocker, ContextShift, IO, Timer}
import org.http4s.client.JavaNetClientBuilder
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.ExecutionContext.global

class TelegramSpec extends AnyFlatSpec {
  implicit val cs: ContextShift[IO] = IO.contextShift(global)
  implicit val timer: Timer[IO] = IO.timer(global)

  private val blockingPool = Executors.newCachedThreadPool()
  private val blocker = Blocker.liftExecutorService(blockingPool)
  private val client = JavaNetClientBuilder[IO](blocker).create

  val tg: Telegram[IO] = Telegram(System.getenv("tkn"), System.getenv("chat"))
  val tgSender: TweetData => IO[String] = tg.sendTweetMessage(client)
  val i = 12345
  val s = "tester"
  val symbols: List[Symbol] =
    List("AMZN", "SPY", "ES=F").map(s => Symbol(s, client)).mapFilter(_.unsafeRunSync())

  val images = List(
    "http://pbs.twimg.com/media/EavZXMIWkAAb8Q1.jpg",
    "http://pbs.twimg.com/media/EayPGA4XQAA8j7a.jpg"
  )

  val tweetData: TweetData =
    TweetData(i, i, false, "Hi hello to all", i, s, s, s, symbols, List(), List())

  "Telegram" should "send text message if there are no images" in {
    tgSender(tweetData).unsafeRunSync()
  }
  it should "send image if there are some" in {
    tgSender(tweetData.copy(media = images)).unsafeRunSync()
  }
  it should "send image if there is only one" in {
    tgSender(tweetData.copy(media = images.take(1))).unsafeRunSync()
  }
  it should "send message if image fetching fails" in {
    tgSender(tweetData.copy(media = List("http://pbs.twimg.com/media/666.jpg"))).unsafeRunSync()
  }
}
