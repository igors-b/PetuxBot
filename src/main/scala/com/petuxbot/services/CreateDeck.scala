package com.petuxbot.services

import cats.FlatMap
import com.petuxbot.domain.cardContainers.Deck

trait CreateDeck[F[_]] {
  def apply(): F[Deck]
}

object CreateDeck {
  def apply[F[_]: FlatMap](shuffle: Shuffle[F]) : CreateDeck[F] = new CreateDeck[F] {
    override def apply(): F[Deck] = Deck.of(shuffle)
  }
}
