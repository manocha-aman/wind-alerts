package com.uptech.windalerts

import cats.effect.Resource.eval
import cats.effect._
import cats.mtl.Handle
import cats.{Monad, Parallel}
import com.google.auth.oauth2.GoogleCredentials
import com.google.firebase.messaging.FirebaseMessaging
import com.google.firebase.{FirebaseApp, FirebaseOptions}
import com.softwaremill.sttp.quick.backend
import com.typesafe.config.ConfigFactory.parseFileAnySyntax
import com.uptech.windalerts.config._
import com.uptech.windalerts.config.beaches._
import com.uptech.windalerts.config.swellAdjustments.Adjustments
import com.uptech.windalerts.core.{BeachesInfrastructure, Infrastructure, NotificationInfrastructure}
import com.uptech.windalerts.core.notifications.NotificationsService
import com.uptech.windalerts.infrastructure.Environment
import com.uptech.windalerts.infrastructure.Environment.{EnvironmentAsk, EnvironmentIOAsk}
import com.uptech.windalerts.infrastructure.beaches.{WWBackedSwellStatusProvider, WWBackedTidesStatusProvider, WWBackedWindStatusProvider}
import com.uptech.windalerts.infrastructure.endpoints.{NotificationEndpoints, errors}
import com.uptech.windalerts.infrastructure.notifications.FirebaseBasedNotificationsSender
import com.uptech.windalerts.infrastructure.repositories.mongo._
import io.circe.config.parser.{decodePath, decodePathF}
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.{Server => H4Server}
import org.http4s.{Response, Status}

import java.io.{File, FileInputStream}
import scala.util.Try

object SendNotifications extends IOApp {
  implicit val configEnv = new EnvironmentIOAsk(Environment(Repos.acquireDb(sys.env("MONGO_DB_URL"))))

  def createServer[F[_] : EnvironmentAsk : ContextShift : ConcurrentEffect : Timer : Parallel]()(implicit M: Monad[F], H: Handle[F, Throwable]): Resource[F, H4Server] = {

    val appConfig = decodePath[com.uptech.windalerts.config.config.SurfsUp](parseFileAnySyntax(config.getConfigFile("application.conf")), "surfsUp").toTry.get
    val projectId = sys.env("projectId")

    val googleCredentials = firebaseCredentials(config.getSecretsFile(s"firebase/firebase.json"))
    val firebaseOptions = new FirebaseOptions.Builder().setCredentials(googleCredentials).setProjectId(projectId).build
    val app = FirebaseApp.initializeApp(firebaseOptions)
    val notifications = FirebaseMessaging.getInstance

    val beaches = decodePath[Beaches](parseFileAnySyntax(config.getConfigFile("beaches.json")), "surfsUp").toTry.get
    val swellAdjustments = decodePath[Adjustments](parseFileAnySyntax(config.getConfigFile("swellAdjustments.json")), "surfsUp").toTry.get
    val willyWeatherAPIKey = sys.env("WILLY_WEATHER_KEY")

    implicit val beachesInfrastructure = BeachesInfrastructure(
      new WWBackedWindStatusProvider(willyWeatherAPIKey),
      new WWBackedTidesStatusProvider(willyWeatherAPIKey, beaches.toMap()),
      new WWBackedSwellStatusProvider(willyWeatherAPIKey, swellAdjustments))

    val usersRepository = new MongoUserRepository[F]()
    val alertsRepository = new MongoAlertsRepository[F]()
    val usersSessionRepository = new MongoUserSessionRepository[F]()

    val notificationsRepository = new MongoNotificationsRepository[F]()


    val notificationsSender = new FirebaseBasedNotificationsSender[F](notifications, beaches.toMap(), appConfig.notifications)
    implicit val infrastructure = NotificationInfrastructure(beachesInfrastructure,
      alertsRepository, usersRepository, usersSessionRepository, notificationsRepository, notificationsSender)
    val notificationsEndPoints = new NotificationEndpoints[F]

    val  httpApp = notificationsEndPoints.allRoutes()



    for {

      server <- BlazeServerBuilder[F]
        .bindHttp(sys.env("PORT").toInt, "0.0.0.0")
        .withHttpApp(httpApp)
        .withHttpApp(new errors[F].errorMapper(httpApp))
        .withServiceErrorHandler(_ => {
          case e: Throwable =>
            logger.error("Exception ", e)
            M.pure(Response[F](status = Status.InternalServerError))
        })
        .resource
    } yield server
  }

  private def firebaseCredentials(file: File) = {
    import cats.implicits._

    Try(GoogleCredentials.fromStream(new FileInputStream(file)))
      .onError(e => Try(logger.error("Could not load creds from app file", e)))
      .getOrElse(GoogleCredentials.getApplicationDefault)
  }

  def run(args: List[String]): IO[ExitCode] = {
    createServer[IO].use(_ => IO.never).as(ExitCode.Success)
  }
}