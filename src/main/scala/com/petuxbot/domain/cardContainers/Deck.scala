package com.petuxbot.domain.cardContainers

import cats.FlatMap
import cats.data.NonEmptyList
import cats.implicits._
import com.petuxbot.domain.Rank.Ranks
import com.petuxbot.domain.Suit.Suits
import com.petuxbot.domain.Card
import com.petuxbot.services.Shuffle

final case class Deck(cards: List[Card]) extends CardContainer {

  def addCards(cardsToAdd: List[Card]): Deck = this.copy(cards ++ cardsToAdd)

  def removeCards(cardsToRemove: List[Card]): Deck = this.copy(cards.diff(cardsToRemove))

  def deal(hands: List[Hand]): Option[(Deck, List[Hand])] = {
    if (hands.nonEmpty) {
      if (cards.nonEmpty) {
        hands.foldLeft((List.empty[Hand], cards)) {
          case ((hands, cards), hand) =>
            val (taken, rest) = cards.splitAt(Hand.InitialNumberOfCards - hand.cards.size)
            (hands :+ hand.addCards(taken), rest)
        } match {
          case (hands, cards) => (this.copy(cards = cards), hands).some
        }
      } else (this, hands).some
    } else None
  }
}

object Deck {
  lazy val Empty: Deck = Deck(List.empty)

  def of[F[_] : FlatMap](shuffle: Shuffle[F]): F[Deck] = {

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
