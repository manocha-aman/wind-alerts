package com.uptech.windalerts.infrastructure.endpoints

import cats.Parallel
import cats.effect.Sync
import cats.implicits._
import cats.mtl.Handle
import cats.mtl.implicits.toHandleOps
import com.uptech.windalerts.core.{BeachNotFoundError, BeachesInfrastructure, Infrastructure}
import com.uptech.windalerts.core.beaches.Beaches
import com.uptech.windalerts.core.beaches.domain.BeachId
import com.uptech.windalerts.infrastructure.endpoints.codecs._
import com.uptech.windalerts.logger
import fs2.Stream
import org.http4s._
import org.http4s.dsl.Http4sDsl

class BeachesEndpoints[F[_]](implicit infrastructure:BeachesInfrastructure[F], Sync: Sync[F], FR: Handle[F, Throwable], P: Parallel[F]) extends Http4sDsl[F] {
  def allRoutes = HttpRoutes.of[F] {
    case GET -> Root / IntVar(id) / "currentStatus" =>
      getStatus(id)
  }

  private def getStatus( id: Int) = {
    Beaches.getStatus(BeachId(id))
      .flatMap(Ok(_))
      .handle[Throwable]({
        case _@BeachNotFoundError(msg) =>
          Response(status = Status.NotFound).withBodyStream(Stream.emits(msg.getBytes()))
        case e@_=>
            Response(status = Status.InternalServerError).withBodyStream(Stream.emits(s"${e.getMessage()}".getBytes()))
      })
  }

}
