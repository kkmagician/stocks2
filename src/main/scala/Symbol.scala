import cats.implicits._
import cats.effect.{ConcurrentEffect, ContextShift}
import io.circe.Json
import io.circe.parser.parse
import org.http4s.{Method, Request}
import org.http4s.client.Client
import org.http4s.implicits._
import io.circe.optics.JsonPath.root
import org.http4s.headers.{AgentProduct, `User-Agent`}
import org.slf4j.{Logger, LoggerFactory}
import Ops.{DoubleOps, StringOps}

case class Symbol(
  ticker: String,
  quoteType: String,
  marketCap: Long,
  marketState: String,
  currentPrice: Double,
  previousClose: Double,
  marketStatePrice: Double,
  yfRecommendationKey: String,
  open: Double,
  dayLow: Double,
  dayHigh: Double,
  fiftyTwoWeekLow: Double,
  fiftyTwoWeekHigh: Double,
  twoHundredDayAverage: Double,
  fiftyDayAverage: Double
) {
  def calculateDeltaStr(base: Double, current: Double): String = {
    val delta = ((current - base) / base * 100).roundPrec(2)
    ((if (delta > 0) '+' + delta.toString else delta.toString) + '%').escapeMarkdown
  }

  def toTelegramString: String = {
    val deltaStr = calculateDeltaStr(previousClose, currentPrice)
    val currentPriceStr = currentPrice.roundPrec(2).toString.escapeMarkdown
    val baseStr = s"*${ticker.escapeMarkdown}*: $currentPriceStr, `$deltaStr`"

    if ((Set("POST", "PRE") contains marketState) && currentPrice != marketStatePrice) {
      val postDelta = calculateDeltaStr(currentPrice, marketStatePrice)
      val marketStatePrefix = if (marketState.startsWith("PRE")) "PM" else "AH"
      val marketStatePriceStr = marketStatePrice.roundPrec(2).toString.escapeMarkdown
      baseStr ++ s" \\($marketStatePrefix: $marketStatePriceStr, `$postDelta\\)`"
    } else baseStr
  }
}

object Symbol {
  private val pattern = "root.App.main = (.+)\n}\\(this\\)".r
  private val financeRootOptic = root.context.dispatcher.stores.QuoteSummaryStore
  private val priceOptic = financeRootOptic.price
  private val financialDataOptic = financeRootOptic.financialData
  private val quoteTypeOptic = financeRootOptic.quoteType
  private val summaryDetailOptic = financeRootOptic.summaryDetail
  private val esgScoresOptic = financeRootOptic.esgScores

  private val marketCapOptic = root.marketCap.raw.long
  private val preMarketPriceOptic = root.preMarketPrice.raw.double
  private val postMarketPriceOptic = root.postMarketPrice.raw.double

  private val logger: Logger = LoggerFactory.getLogger("log")

  private case class SymbolData(
    price: Json,
    financialData: Option[Json],
    quoteType: Json,
    summaryDetail: Json,
    esgScores: Option[Json]
  ) {
    def toSymbol(ticker: String): Option[Symbol] =
      for {
        marketCap     <- marketCapOptic.getOption(price).orElse(Some(0L))
        quoteType     <- root.quoteType.string.getOption(quoteType)
        marketState   <- root.marketState.string.getOption(price)
        currentPrice  <- root.regularMarketPrice.raw.double.getOption(price)
        previousClose <- root.regularMarketPreviousClose.raw.double.getOption(price)
        marketStatePrice <- marketState match {
                             case ("POST" | "POSTPOST") =>
                               postMarketPriceOptic.getOption(price).orElse(Some(currentPrice))
                             case ("PRE" | "PREPRE") =>
                               preMarketPriceOptic.getOption(price).orElse(Some(currentPrice))
                             case _ => Some(currentPrice)
                           }
        rec     <- financialData.flatMap(root.recommendationKey.string.getOption).orElse(Some(""))
        open    <- root.open.raw.double.getOption(summaryDetail).orElse(Some(currentPrice))
        dayLow  <- root.dayLow.raw.double.getOption(summaryDetail).orElse(Some(currentPrice))
        dayHigh <- root.dayHigh.raw.double.getOption(summaryDetail).orElse(Some(currentPrice))
        fiftyTwoWeekLow <- root.fiftyTwoWeekLow.raw.double
                            .getOption(summaryDetail)
                            .orElse(Some(currentPrice))
        fiftyTwoWeekHigh <- root.fiftyTwoWeekHigh.raw.double
                             .getOption(summaryDetail)
                             .orElse(Some(currentPrice))
        twoHundredDayAverage <- root.twoHundredDayAverage.raw.double
                                 .getOption(summaryDetail)
                                 .orElse(Some(currentPrice))
        fiftyDayAverage <- root.fiftyDayAverage.raw.double
                            .getOption(summaryDetail)
                            .orElse(Some(currentPrice))
      } yield
        Symbol(
          ticker,
          quoteType,
          marketCap,
          marketState,
          currentPrice,
          previousClose,
          marketStatePrice,
          rec,
          open,
          dayLow,
          dayHigh,
          fiftyTwoWeekLow,
          fiftyTwoWeekHigh,
          twoHundredDayAverage,
          fiftyDayAverage
        )
  }

  private def makeSymbolData(data: Json): Option[SymbolData] =
    List(priceOptic, financialDataOptic, quoteTypeOptic, summaryDetailOptic, esgScoresOptic).map(
      _.json.getOption(data)
    ) match {
      case List(Some(p), f, Some(q), Some(s), e) => Some(SymbolData(p, f, q, s, e))
      case _                                     => None
    }

  private def dataJson(data: String) =
    pattern
      .findFirstMatchIn(data)
      .map(_.toString.drop(16).dropRight(9))
      .flatMap(parse(_).toOption)
      .flatMap(makeSymbolData)

  def apply[F[_]: ConcurrentEffect: ContextShift](
    ticker: String,
    client: Client[F]
  ): F[Option[Symbol]] = {
    val request =
      Request[F](
        Method.GET,
        uri"https://finance.yahoo.com".withPath("/quote/" ++ ticker),
      ).withHeaders(`User-Agent`(AgentProduct("Mozilla/5.0")))

    client
      .expectOr[String](request)(_.as[String].map(err => new Throwable(err)))
      .map(dataJson)
      .attemptT
      .value
      .map {
        case Right(Some(sd)) => sd.toSymbol(ticker)
        case Right(None)     => logger.info(s"No data for $ticker"); None
        case Left(exc) =>
          logger.error(s"Could not extract data for $ticker with ${exc.getMessage}")
          None
      }
  }
}
