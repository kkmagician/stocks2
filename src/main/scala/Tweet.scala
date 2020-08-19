import cats.implicits._
import io.circe.Json
import io.circe.optics.JsonPath._

case class Tweet(
  id: Long,
  createdAt: Long,
  truncated: Boolean,
  text: String,
  userId: Long,
  userName: String,
  replyTo: String,
  retweetedFrom: String,
  symbols: List[String],
  media: List[String],
  hashtags: List[String]
) {
  def toTweetData(ss: List[Symbol]): TweetData =
    TweetData(
      id,
      createdAt,
      truncated,
      text,
      userId,
      userName,
      replyTo,
      retweetedFrom,
      ss,
      media,
      hashtags
    )
}

object Tweet {
  private val symbolPattern = "\\$[a-zA-Z_]{1,5}".r
  private val rtPattern = "RT @[^:]+: ".r
  private val hashtagPattern = "#[\\w\\d]+".r

  private val mentionPattern = "@[\\w_]+\\s?"
  private val linkPattern = "\\s?https://t.co/[\\w]{10}"

  private val hashtagAvailableSymbols = List("ES_F", "SPY").map("#" ++ _)

  private val truncatedOptic = root.truncated.boolean
  private val fullTextOptic = root.full_text.string
  private val textOptic = root.text.string
  private val userOptic = root.user
  private val userIdOptic = userOptic.id.long
  private val userNameOptic = userOptic.screen_name.string

  private val extendedTweetOptic = root.extended_tweet
  private val entitiesOptic = root.entities
  private val symbolEntitiesOptic = entitiesOptic.symbols.each.text.string

  private val mediaEntitiesOptic = entitiesOptic.media.each.media_url.string
  private val extendedEntitiesOptic = root.entities.media.each.media_url.string

  private val retweetedFromOptic = root.retweeted_status.user.screen_name.string

  def apply(j: Json): Option[Tweet] = {
    val (text, symbols, media) = extendedTweetOptic.json.getOption(j) match {
      case Some(extended) =>
        (
          fullTextOptic.getOption(extended),
          symbolEntitiesOptic.getAll(extended),
          extendedEntitiesOptic.getAll(extended)
        )
      case _ =>
        (textOptic.getOption(j), symbolEntitiesOptic.getAll(j), mediaEntitiesOptic.getAll(j))
    }

    val tweetId = root.id.long.getOption(j)
    val createdAt = root.timestamp_ms.string.getOption(j).flatMap(_.toLongOption).map(_ / 1000)
    val userId = userIdOptic.getOption(j)
    val userName = userNameOptic.getOption(j)
    val truncated = truncatedOptic.getOption(j)
    val replyTo = root.in_reply_to_screen_name.string.getOption(j).getOrElse("")
    val retweetedFrom = retweetedFromOptic.getOption(j).getOrElse("")
    val textCleaned = text.map(rtPattern.replaceFirstIn(_, ""))

    (tweetId, createdAt, truncated, textCleaned, userId, userName).mapN {
      case (id, created, trunc, txt, uid, name) =>
        val hashtags = hashtagPattern.findAllIn(txt).toList.distinct.sorted
        val hashtagSymbols =
          hashtags.map(_.toUpperCase).intersect(hashtagAvailableSymbols).map(_.drop(1))

        val cleanedText = List(mentionPattern, linkPattern)
          .fold(txt)({
            case (acc, add) => acc.replaceAll(add, "")
          })
          .trim

        Tweet(
          id,
          created,
          trunc,
          cleanedText,
          uid,
          name,
          replyTo,
          retweetedFrom,
          symbols ++ symbolPattern.findAllIn(cleanedText).map(_.drop(1)).toList ++ hashtagSymbols,
          media,
          hashtags
        )
    }
  }
}
