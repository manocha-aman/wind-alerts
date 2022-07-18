package com.uptech.windalerts.infrastructure.endpoints

import cats.data.OptionT
import cats.effect.Effect
import cats.implicits._
import cats.mtl.Handle
import cats.mtl.implicits.toHandleOps
import com.uptech.windalerts.core.alerts.{AlertRequest, AlertsService}
import com.uptech.windalerts.core.types._
import com.uptech.windalerts.core.user.UserIdMetadata
import com.uptech.windalerts.core.{AlertNotFoundError, Infrastructure, OperationNotAllowed}
import com.uptech.windalerts.infrastructure.endpoints.codecs._
import com.uptech.windalerts.infrastructure.endpoints.errors.mapError
import fs2.Stream
import org.http4s.dsl.Http4sDsl
import org.http4s.{AuthedRoutes, Response, Status}


class AlertsEndpoints[F[_] : Effect](implicit infrastructure:Infrastructure[F], FR: Handle[F, Throwable]) extends Http4sDsl[F] {
  def allUsersService(): AuthedRoutes[UserIdMetadata, F] =
    AuthedRoutes {

      case _@GET -> Root as user => {
        OptionT.liftF(
          for {
            response <- AlertsService.getAllForUser(user.userId.id).map(alerts => AlertsDTO(alerts))
            resp <- Ok(response)
          } yield resp)
      }

      case _@DELETE -> Root / alertId as user => {
        OptionT.liftF(AlertsService.delete(user.userId.id, alertId)
          .flatMap(_ => NoContent())
          .handle[Throwable]({
            case _@AlertNotFoundError(_) =>
              Response(status = Status.NotFound).withBodyStream(Stream.emits("Alert not found".getBytes()))
          }))
      }

      case authReq@PUT -> Root / alertId as user => {
        OptionT.liftF(authReq.req.decode[AlertRequest] {
          request =>
            AlertsService.update(alertId, user.userId, user.userType, request)
              .flatMap(Ok(_))
              .handle[Throwable]({
                case _@AlertNotFoundError(msg) =>
                  Response(status = Status.NotFound).withBodyStream(Stream.emits("Alert not found".getBytes()))
                case _@OperationNotAllowed(msg) =>
                  Response(status = Status.Forbidden).withBodyStream(Stream.emits(msg.getBytes()))
              })
        })
      }
      case authReq@POST -> Root as user => {
        OptionT.liftF(authReq.req.decode[AlertRequest] {
          request =>
            (for {
              response <- AlertsService.createAlert(user.userId, user.userType, request)
            } yield response).flatMap(
              Created(_)
            ).handle[Throwable](mapError(_))
        })
      }

    }

}
