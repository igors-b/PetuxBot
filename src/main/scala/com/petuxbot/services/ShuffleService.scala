package com.petuxbot.services

import cats.effect.Sync

import scala.collection.BuildFrom
import scala.util.Random

trait ShuffleService[F[_]] {
  def apply[T, C](xs: IterableOnce[T])(implicit bf: BuildFrom[xs.type, T, C]): F[C]
}
object ShuffleService {
  def apply[F[_]: Sync]: ShuffleService[F] = new ShuffleService[F] {
    def apply[T, C](xs: IterableOnce[T])(implicit bf: BuildFrom[xs.type, T, C]): F[C] =
      Sync[F].delay(Random.shuffle(xs))
  }
}
