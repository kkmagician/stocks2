import cats.implicits._
import cats.data.EitherT
import cats.effect.{IO, Resource}

import scala.io.Source
import scala.math.pow

object Ops {
  def readDockerSecret(key: String): EitherT[IO, String, String] = {
    val fileResource =
      Resource.fromAutoCloseable(IO(Source.fromFile(s"/run/secrets/$key")))
    fileResource.use(s => IO(s.mkString)).attemptT.leftMap(_.getMessage)
  }

  def readEnv(key: String): EitherT[IO, String, String] =
    EitherT(IO {
      System.getenv(key) match {
        case null  => Left(s"No key $key found")
        case value => Right(value)
      }
    })

  def readUsersList(location: String): EitherT[IO, String, List[String]] = {
    val fileResource =
      Resource.fromAutoCloseable(IO(Source.fromFile(location)))
    fileResource.use(s => IO(s.getLines.toList)).attemptT.leftMap(_.getMessage)
  }

  def tryFindKey(
    key: String,
    default: Option[String] = None
  ): EitherT[IO, String, String] = {
    val run = readDockerSecret(key).recoverWith(_ => readEnv(key))

    default match {
      case Some(d) => run.recover(_ => d)
      case None    => run
    }
  }

  implicit class DoubleOps(d: Double) {
    def roundPrec(digits: Int): Double = {
      val div = pow(10, digits)
      (d * div).round.toDouble / div
    }
  }

  implicit class StringOps(s: String) {
    def escapeMarkdown: String =
      s.flatMap({
        case c
            if Set('_', '*', '[', ']', '(', ')', '~', '`', '>', '#', '+', '-', '=', '|', '{', '}',
              '.', '!').contains(c) =>
          s"\\$c"
        case c @ _ => c.toString
      })
  }
}
