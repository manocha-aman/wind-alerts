package com.uptech.windalerts.infrastructure.endpoints

import cats.Parallel
import cats.effect.Effect
import cats.mtl.Handle
import com.uptech.windalerts.core.{BeachesInfrastructure, Infrastructure, NotificationInfrastructure}
import com.uptech.windalerts.core.notifications.NotificationsService
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._

class NotificationEndpoints[F[_] : Effect](implicit  notificationInfrastructure:NotificationInfrastructure[F], FR: Handle[F, Throwable], P:Parallel[F]) extends Http4sDsl[F] {
  def allRoutes() =
    routes().orNotFound

  def routes() = {
    import cats.syntax.flatMap._

    HttpRoutes.of[F] {
      case GET -> Root / "notify" => {
        NotificationsService.sendNotification().flatMap {
          _ => Ok()
        }
      }


      case GET -> Root => {
        NotificationsService.sendNotification().flatMap {
          _ => Ok()
        }
      }
    }


  }
}