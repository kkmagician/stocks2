import cats.effect.{Blocker, ConcurrentEffect, ContextShift, IO, Timer}
import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import java.util.concurrent._

import scala.concurrent.ExecutionContext.global
import org.http4s.client.{Client, JavaNetClientBuilder}
import org.slf4j.LoggerFactory

class SymbolSpec extends AnyFlatSpec {
  private val logger = LoggerFactory.getLogger(this.getClass)
  implicit val cs: ContextShift[IO] = IO.contextShift(global)
  implicit val timer: Timer[IO] = IO.timer(global)

  private val blockingPool = Executors.newFixedThreadPool(2)
  private val blocker = Blocker.liftExecutorService(blockingPool)
  val httpClient: Client[IO] = JavaNetClientBuilder[IO](blocker).create

  private def checkSymbol[F[_]: ConcurrentEffect: ContextShift](s: F[Option[Symbol]]) =
    s.map(v => {
      if (v.isDefined) {
        logger.info(v.get.toString)
        logger.info(v.get.toTelegramString)
        succeed
      } else fail("Symbol is not defined")
    })

  "A symbol" should "be created from a stock ticker" in {
    checkSymbol(Symbol("AMZN", httpClient)).unsafeRunSync()
  }
  it should "be created from a ETF" in {
    checkSymbol(Symbol("SPY", httpClient)).unsafeRunSync()
  }
  it should "be created from a future" in {
    checkSymbol(Symbol("ES=F", httpClient)).unsafeRunSync()
  }
  it should "be created from lowercase or mixed case" in {
    List(
      checkSymbol(Symbol("es=F", httpClient)),
      checkSymbol(Symbol("spy", httpClient)),
      checkSymbol(Symbol("AmZn", httpClient))
    ).sequence.unsafeRunSync()
  }

  "Deltas" should "be calculated in AH with POST" in {
    val s = Symbol("AMZN", httpClient).unsafeRunSync()

    s.map(_.copy(marketState = "POST").toTelegramString) match {
      case Some(value) if value.contains("AH:") => logger.info(value); succeed
      case Some(value) if s.map(_.marketStatePrice) == s.map(_.currentPrice) =>
        logger.info(value); succeed
      case Some(value) => logger.info(value); fail("No AH was found")
      case None        => fail("Symbol is not defined")
    }
  }
  it should "be calculated in PM with PRE" in {
    val s = Symbol("MSFT", httpClient).unsafeRunSync()

    s.map(_.copy(marketState = "PRE").toTelegramString) match {
      case Some(value) if value.contains("PM:") => logger.info(value); succeed
      case Some(value) if s.map(_.marketStatePrice) == s.map(_.currentPrice) =>
        logger.info(value); succeed
      case Some(value) => logger.info(value); fail("No PM was found")
      case None        => fail("Symbol is not defined")
    }
  }
}
