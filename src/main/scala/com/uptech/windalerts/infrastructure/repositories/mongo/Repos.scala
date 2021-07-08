package com.uptech.windalerts.infrastructure.repositories.mongo

import cats.Eval
import cats.effect.{ContextShift, IO}
import com.google.api.services.androidpublisher.AndroidPublisher
import com.uptech.windalerts.config.secrets
import com.uptech.windalerts.core.alerts.AlertsRepository
import com.uptech.windalerts.core.alerts.domain.Alert
import com.uptech.windalerts.core.notifications.{Notification, NotificationRepository}
import com.uptech.windalerts.core.social.login.{AppleAccessRequest, FacebookAccessRequest, SocialLogin}
import com.uptech.windalerts.infrastructure.endpoints.codecs
import com.uptech.windalerts.infrastructure.social.login.{AppleLogin, FacebookLogin}
import com.uptech.windalerts.infrastructure.social.subscriptions.{AndroidPublisherHelper, ApplicationConfig}
import org.mongodb.scala.{MongoClient, MongoDatabase}

import java.io.File

trait Repos[F[_]] {
  def fbSecret() : String

  def applePlatform(): SocialLogin[F, AppleAccessRequest]

  def facebookPlatform(): SocialLogin[F, FacebookAccessRequest]

}

object Repos{
  def acquireDb() = {
    val start = System.currentTimeMillis()
    val value = Eval.later {
      val client = MongoClient(com.uptech.windalerts.config.secrets.read.surfsUp.mongodb.url)
      client.getDatabase(sys.env("projectId")).withCodecRegistry(codecs.codecRegistry)
    }
    val v = value.value
    println(System.currentTimeMillis() - start)
    v
  }
}
class LazyRepos(implicit cs: ContextShift[IO]) extends Repos[IO] {




  val fbKey = Eval.later {
    secrets.read.surfsUp.facebook.key
  }


  override  def fbSecret = fbKey.value

  override def applePlatform(): SocialLogin[IO, AppleAccessRequest] = {
    if (new File(s"/app/resources/Apple-${sys.env("projectId")}.p8").exists())
      new AppleLogin(s"/app/resources/Apple-${sys.env("projectId")}.p8")
    else new AppleLogin(s"src/main/resources/Apple.p8")
  }

  override def facebookPlatform(): SocialLogin[IO, FacebookAccessRequest] = new FacebookLogin(fbKey.value)
}
