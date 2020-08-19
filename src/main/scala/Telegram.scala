import java.awt.image.BufferedImage
import java.net.URL

import cats.data.Chain
import cats.effect.{Blocker, Concurrent, ConcurrentEffect, ContextShift, Timer}
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.{Method, Request}
import org.http4s._
import org.http4s.circe._
import org.http4s.client.Client
import cats.implicits._
import fs2.Stream
import fs2.io.readOutputStream
import javax.imageio.ImageIO
import java.io.OutputStream

import org.http4s.implicits._
import org.http4s.multipart.{Multipart, Part}
import org.slf4j.LoggerFactory

import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration.fromNanos
import scala.util.Try
import scala.math.pow

case class MessageGroup(chat_id: String, media: Vector[Media])
case class Media(
  media: String,
  caption: Option[String] = None,
  parse_mode: Option[String] = None,
  `type`: String = "photo"
)

case class Telegram[F[_]: ConcurrentEffect: ContextShift](botToken: String, chatId: String) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  private val baseForm = Vector(
    ("chat_id", chatId),
    ("parse_mode", "MarkdownV2"),
    ("disable_web_page_preview", "true")
  )

  private val baseRequest = Request[F](
    Method.POST,
    uri"https://api.telegram.org" / ("bot" ++ botToken)
  )

  private def createTextRequest(text: String): Request[F] = {
    val form = UrlForm.fromChain(Chain.fromSeq(baseForm :+ ("text", text)))
    baseRequest.withEntity(form).withUri(baseRequest.uri / "sendMessage")
  }

  private def createMediaGroupRequest(caption: String, media: Seq[String]): Request[F] = {
    val mediaGroup = media.toVector.mapWithIndex({
      case (url, idx) if idx == 0 => Media(url, Some(caption), Some("MarkdownV2"))
      case (url, _)               => Media(url)
    })

    val message = MessageGroup(chatId, mediaGroup)

    baseRequest
      .withEntity(message.asJson.deepDropNullValues)
      .withUri(baseRequest.uri / "sendMediaGroup")
  }

  @deprecated
  private def createImageRequest(caption: String, image: Stream[F, Byte]): Request[F] = {
    val form = (baseForm :+ ("caption", caption)).map({ case (n, v) => Part.formData[F](n, v) })
    val imagePart = Part.fileData("photo", "photo.png", image)
    val multipart = Multipart[F](imagePart +: form)

    baseRequest
      .withEntity(multipart)
      .withHeaders(multipart.headers)
      .withUri(baseRequest.uri / "sendPhoto")
  }

  @deprecated
  private def makeBufferImage(images: Seq[BufferedImage], width: Int, heights: Seq[Int])(
    implicit eff: Concurrent[F]
  ) =
    eff.delay {
      val height = heights.sum
      val heightCum = heights
        .foldLeft(List(0))({ case (acc: List[Int], add) => acc :+ (add + acc.last) })
        .dropRight(1)
      val res = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
      val graphics = res.getGraphics
      images.zip(heightCum).foreach({ case (img, y) => graphics.drawImage(img, 0, y, null) })
      graphics.dispose()
      res
    }

  @deprecated
  private def createImageStream(paths: Seq[String], blocker: Blocker)(
    implicit eff: Concurrent[F]
  ): Stream[F, Byte] = {
    val makeImage = (s: OutputStream) =>
      for {
        urls          <- eff.delay(paths.map(p => new URL(p)))
        images        <- eff.delay(urls.toVector.mapFilter((u: URL) => Try(ImageIO.read(u)).toOption))
        width         <- eff.delay(images.map(_.getWidth).max)
        heights       <- eff.delay(images.map(_.getHeight))
        bufferedImage <- makeBufferImage(images, width, heights)
        _             <- eff.delay(ImageIO.write(bufferedImage, "png", s))
      } yield ()

    readOutputStream[F](blocker, 4096)(makeImage)
  }

  private def sendRequest(client: Client[F])(request: Request[F])(
    implicit eff: Concurrent[F],
    timer: Timer[F]
  ): F[String] =
    client.expectOr[String](request)(err => err.as[String].map(new Throwable(_)))

  private def sendMessage(
    client: Client[F]
  )(text: String)(implicit eff: Concurrent[F], timer: Timer[F]): F[String] =
    sendRequest(client)(createTextRequest(text))

  @deprecated
  private def sendPhoto(client: Client[F])(
    caption: String,
    image: Stream[F, Byte]
  )(implicit eff: Concurrent[F], timer: Timer[F]): F[String] = {
    val timeout = timer
      .sleep(fromNanos(pow(10, 10).toLong))
      .map(_ => new TimeoutException("Image extraction has taken too long"))

    eff.race(timeout, sendRequest(client)(createImageRequest(caption, image))).flatMap {
      case Right(resp) => eff.pure(resp)
      case Left(err) =>
        logger.error(err.getMessage)
        sendMessage(client)(caption)
    }
  }

  def sendTweetMessage(
    client: Client[F]
  )(tweetData: TweetData)(implicit timer: Timer[F]): F[String] = {
    val text = tweetData.toTelegramMessage
    if (tweetData.media.nonEmpty) {
      val message = createMediaGroupRequest(text, tweetData.media)
      sendRequest(client)(message).recoverWith(_ => sendMessage(client)(text))
    } else sendMessage(client)(text)
  }
}
