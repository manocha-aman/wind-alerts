package com.uptech.windalerts.core.beaches

import cats.implicits._
import cats.mtl.Raise
import cats.{Monad, Parallel}
import com.uptech.windalerts.core.{BeachNotFoundError, BeachesInfrastructure, Infrastructure}
import com.uptech.windalerts.core.beaches.domain._


object Beaches {
  def getStatus[F[_]](beachId: BeachId)(implicit infrastructure: BeachesInfrastructure[F], M: Monad[F]
                                        , P: Parallel[F]
                                        , FR: Raise[F, BeachNotFoundError]
  ): F[Beach] = {
    (infrastructure.windStatusProvider.get(beachId),
      infrastructure.tideStatusProvider.get(beachId),
      infrastructure.swellStatusProvider.get(beachId))
      .parMapN(
        (wind, tide, swell) =>
          Beach(
            beachId,
            wind,
            Tide(tide, SwellOutput(swell.height, swell.direction, swell.directionText)))
      )
  }

  def getAll[F[_]](beachIds: Seq[BeachId])(implicit infrastructure: BeachesInfrastructure[F], M: Monad[F], P: Parallel[F], FR: Raise[F, BeachNotFoundError]): F[Map[BeachId, Beach]] =
    beachIds.traverse(getStatus(_)).map(beaches => beaches.map(beach => (beach.beachId, beach)).toMap)
}