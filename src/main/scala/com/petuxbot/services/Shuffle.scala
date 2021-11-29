package com.petuxbot.services

import cats.effect.Sync

import scala.collection.BuildFrom
import scala.util.Random

trait Shuffle[F[_]] {
  def apply[T, C](xs: IterableOnce[T])(implicit bf: BuildFrom[xs.type, T, C]): F[C]
}
object Shuffle {
  def apply[F[_]: Sync]: Shuffle[F] = new Shuffle[F] {
    def apply[T, C](xs: IterableOnce[T])(implicit bf: BuildFrom[xs.type, T, C]): F[C] =
      Sync[F].delay(Random.shuffle(xs))
  }
}
