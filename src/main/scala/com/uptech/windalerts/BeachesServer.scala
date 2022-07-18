package com.uptech.windalerts

import cats.effect.Resource.eval
import cats.effect._
import cats.mtl.Handle
import cats.{Applicative, Parallel}
import com.softwaremill.sttp.quick.backend
import com.typesafe.config.ConfigFactory.parseFileAnySyntax
import com.uptech.windalerts.config._
import com.uptech.windalerts.config.beaches._
import com.uptech.windalerts.config.swellAdjustments.Adjustments
import com.uptech.windalerts.core.BeachesInfrastructure
import com.uptech.windalerts.core.beaches.Beaches
import com.uptech.windalerts.infrastructure.beaches.{WWBackedSwellStatusProvider, WWBackedTidesStatusProvider, WWBackedWindStatusProvider}
import com.uptech.windalerts.infrastructure.endpoints.{BeachesEndpoints, errors}
import io.circe.config.parser.{decodePath, decodePathF}
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits._
import org.http4s.server.{Router, Server => H4Server}
import org.http4s.{Response, Status}

import scala.concurrent.ExecutionContext

object BeachesServer extends IOApp {
  def createServer[F[_]](implicit Sync: Sync[F], H: Handle[F, Throwable], AS: Async[F], CE: ConcurrentEffect[F], CS: ContextShift[F], T: Timer[F], P: Parallel[F]): Resource[F, H4Server] = {
    val beaches = decodePath[Beaches](parseFileAnySyntax(config.getConfigFile("beaches.json")), "surfsUp").toTry.get
    val swellAdjustments = decodePath[Adjustments](parseFileAnySyntax(config.getConfigFile("swellAdjustments.json")), "surfsUp").toTry.get
    val willyWeatherAPIKey = sys.env("WILLY_WEATHER_KEY")
    implicit val beachesInfrastructure = BeachesInfrastructure[F](
      new WWBackedWindStatusProvider[F](willyWeatherAPIKey),
      new WWBackedTidesStatusProvider[F](willyWeatherAPIKey, beaches.toMap()),
      new WWBackedSwellStatusProvider[F](willyWeatherAPIKey, swellAdjustments))
    val httpApp = Router(
      "/v1/beaches" -> new BeachesEndpoints[F].allRoutes
    ).orNotFound

    for {
      server <- BlazeServerBuilder(ExecutionContext.global)
        .bindHttp(sys.env("PORT").toInt, "0.0.0.0")
        .withHttpApp(httpApp)
        .withHttpApp(new errors[F].errorMapper(httpApp))
        .withServiceErrorHandler(_ => {
          case e: Throwable =>
            logger.error("Exception ", e)
            Applicative[F].pure(Response[F](status = Status.InternalServerError))
        })
        .resource
    } yield server
  }

  override def run(args: List[String]): IO[ExitCode] = {
    createServer[IO].use(_ => IO.never).as(ExitCode.Success)
  }

}

