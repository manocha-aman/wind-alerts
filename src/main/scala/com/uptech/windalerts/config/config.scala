package com.uptech.windalerts.config

import cats.effect.Sync
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import org.http4s.EntityDecoder
import org.http4s.circe.jsonOf

import java.io.File
import scala.io.Source
import scala.util.Try
import io.circe.generic.semiauto.deriveDecoder

object config {
  implicit val surfsUpDecoder: Decoder[SurfsUp] = deriveDecoder

  implicit def surfsUpDntityDecoder[F[_] : Sync]: EntityDecoder[F, SurfsUp] = jsonOf

  implicit val notificationsDecoder: Decoder[Notifications] = deriveDecoder

  implicit def notificationsEntityDecoder[F[_] : Sync]: EntityDecoder[F, Notifications] = jsonOf

  def getConfigFile(name: String): File = {
    val prodFile = new File(s"/app/resources/$name")
    val localFile = new File(s"src/main/resources/$name")

    if (prodFile.exists()) prodFile
    else localFile
  }

  def getSecretsFile(name: String): File = {
    new File(s"src/main/resources/secrets/$name")
  }

  case class AppConfig(surfsUp: SurfsUp)

  case class SurfsUp(notifications: Notifications)

  case class Notifications(title: String, body: String)

}

object swellAdjustments {

  case class Adjustments(adjustments: Seq[Adjustment]) {
    def adjust(height: Double): Double = {
      height *
        adjustments
          .filter(adjustment => adjustment.from <= height && adjustment.to >= height)
          .headOption.map(_.factor).getOrElse(1.0)
    }

  }

  case class Adjustment(from: Double, to: Double, factor: Double)

  implicit val adjustmentDecoder: Decoder[Adjustment] = deriveDecoder
  implicit val adjustmentsDecoder: Decoder[Adjustments] = deriveDecoder

}

object statics {
  def read(name: String) = {
    Try(Source.fromFile(s"/app/resources/$name.md").getLines.mkString)
      .getOrElse(Source.fromFile(s"src/main/resources/$name.md").getLines.mkString)
  }

  def privacyPolicy = read("privacy-policy")

  def aboutSurfsUp = read("about-surfs-up")
}

object beaches {
  case class Beaches(beaches: Seq[Beach]) {
    def toMap() = {
      beaches.groupBy(_.id).toMap.mapValues(_.head).toMap
    }
  }

  case class Beach(id: Long, location: String, postCode: Long, region: String)

  implicit val beachesDecoder: Decoder[Beaches] = deriveDecoder

  implicit def beachesEntityDecoder[F[_] : Sync]: EntityDecoder[F, Beaches] = jsonOf

  implicit val beachDecoder: Decoder[Beach] = deriveDecoder

  implicit def beachEntityDecoder[F[_] : Sync]: EntityDecoder[F, Beach] = jsonOf

}

object secrets {
  implicit val willyWeatherDecoder: Decoder[WillyWeather] = deriveDecoder

  implicit def willyWeatherEntityDecoder[F[_] : Sync]: EntityDecoder[F, WillyWeather] = jsonOf

  implicit val facebookDecoder: Decoder[Facebook] = deriveDecoder

  implicit def facebookEntityDecoder[F[_] : Sync]: EntityDecoder[F, Facebook] = jsonOf

  implicit val appleDecoder: Decoder[Apple] = deriveDecoder

  implicit def appleEntityDecoder[F[_] : Sync]: EntityDecoder[F, Apple] = jsonOf

  implicit val emailDecoder: Decoder[Email] = deriveDecoder

  implicit def emailEntityDecoder[F[_] : Sync]: EntityDecoder[F, Email] = jsonOf

  implicit val mongodbDecoder: Decoder[Mongodb] = deriveDecoder

  implicit def mongodbEntityDecoder[F[_] : Sync]: EntityDecoder[F, Mongodb] = jsonOf

  implicit val surfsUpSecretDecoder: Decoder[SurfsUpSecret] = deriveDecoder

  implicit def surfsUpSecretDntityDecoder[F[_] : Sync]: EntityDecoder[F, SurfsUpSecret] = jsonOf

  case class SecretsSettings(surfsUp: SurfsUpSecret)

  case class SurfsUpSecret(willyWeather: WillyWeather, facebook: Facebook, apple: Apple, email: Email, mongodb: Mongodb)

  case class WillyWeather(key: String)

  case class Facebook(key: String)

  case class Apple(appSecret: String)

  case class Email(apiKey: String)

  case class Mongodb(url: String)

  def getConfigFile(): File = {
    val projectId = sys.env("projectId")
    val prodFile = new File(s"/app/resources/secrets-$projectId.conf")
    if (prodFile.exists()) {
      prodFile
    }
    else {
      new File("src/main/resources/secrets.conf")
    }
  }
}