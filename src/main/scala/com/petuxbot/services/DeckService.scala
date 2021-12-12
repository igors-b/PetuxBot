package com.petuxbot.services

import cats.FlatMap
import cats.data.NonEmptyList
import cats.implicits._
import com.petuxbot.domain.Card
import com.petuxbot.domain.Rank.Ranks
import com.petuxbot.domain.Suit.Suits
import com.petuxbot.domain.cardcontainers.Deck

trait DeckService[F[_]] {
  def of: F[Deck]
}

object DeckService {
  def apply[F[_]: FlatMap](shuffle: ShuffleService[F]) : DeckService[F] = new DeckService[F] {
    def of: F[Deck] = {

      val allCards: NonEmptyList[Card] = Ranks
        .flatMap(rank => Suits.map(suit => Card(rank, suit)))

      for {
        shuffledCards           <- shuffle(allCards.toList)
        shuffleCardsNel         = NonEmptyList.fromListUnsafe(shuffledCards)
        trumpCard               = shuffleCardsNel.last.copy(isTrump = true)
        shuffledCardsWithTrumps = shuffledCards
          .map(card => if (card.suit == trumpCard.suit) card.copy(isTrump = true) else card)
      } yield Deck(shuffledCardsWithTrumps)
    }
  }
}
