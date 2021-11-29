package com.petuxbot.domain

import cats.data.NonEmptyList
import scala.util.Random
import cats.implicits._
import com.petuxbot.domain.Rank.Ranks
import com.petuxbot.domain.Suit.Suits

final case class Deck(cards: List[Card]) extends CardContainer {
  override def addCard(card: Card): Deck = this.copy(cards :+ card)

  override def removeCard(card: Card): Deck =
    this.copy(cards filterNot (c => c == card))

   def deal(hands: List[Hand]): Option[(Deck, List[Hand])] =
   {
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
  lazy val Empty = Deck(List.empty)

  def make: Deck = {

    val allCards: NonEmptyList[Card] = Ranks
      .flatMap(rank =>
        Suits.map(suit =>
          Card(rank, suit)
        )
      )

    val shuffledCards = Random.shuffle(allCards.toList)
    val shuffleCardsNel = NonEmptyList.fromListUnsafe(shuffledCards)
    val trumpCard = shuffleCardsNel.last.copy(isTrump = true)


    val shuffledCardsWithTrumps =
      shuffledCards.map(card =>
        if (card.suit == trumpCard.suit) card.copy(isTrump = true) else card
      )

    Deck(shuffledCardsWithTrumps)
  }
}