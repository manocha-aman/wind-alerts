package com.uptech.windalerts

import cats.effect.{ContextShift, IO}
import cats.mtl.Raise
import com.uptech.windalerts.core.{BeachNotFoundError, SurfsUpError}
import com.uptech.windalerts.core.beaches._
import com.uptech.windalerts.core.beaches.domain._
import com.uptech.windalerts.infrastructure.endpoints.codecs._
import org.http4s._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.ExecutionContext

class BeachesEndpointsSpec extends AnyFunSuite
  with Matchers
  with ScalaCheckPropertyChecks
  with Arbitraries
  with Http4sDsl[IO]
  with Http4sClientDsl[IO] {
  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  test("fetch wind swell and tide status") {
    forAll { (w: Wind, t: TideHeight, s: Swell) =>
      (
        for {
          response <- beachEndPoints(w, t, s).orNotFound.run(Request(method = Method.GET, uri = uri"/1/currentStatus"))
          status <- response.as[Beach]
        } yield status shouldBe Beach(BeachId(1), w, Tide(t, SwellOutput(s.height, s.direction, s.directionText)))
        ).unsafeRunSync()
    }
  }

  private def beachEndPoints(w: Wind, t: TideHeight, s: Swell) = {
    new com.uptech.windalerts.infrastructure.endpoints.BeachesEndpoints[IO](new Beaches[IO](new FixedWindService(w), new FixedTidesStatusProvider(t), new FixedSwellService(s))).allRoutes
  }

  class FixedWindService(wind: Wind) extends WindStatusProvider[IO] {
    override def get(beachId: domain.BeachId)(implicit FR: Raise[IO, BeachNotFoundError]) = IO.pure(wind)
  }

  class FixedTidesStatusProvider(tideHeight: TideHeight) extends TidesStatusProvider[IO] {
    override def get(beachId: domain.BeachId)(implicit FR: Raise[IO, BeachNotFoundError]) = IO.pure(tideHeight)
  }

  class FixedSwellService(swell: Swell) extends SwellStatusProvider[IO] {
    override def get(beachId: domain.BeachId)(implicit FR: Raise[IO, BeachNotFoundError]) = IO.pure(swell)
  }
}