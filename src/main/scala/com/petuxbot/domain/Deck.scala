package com.petuxbot.domain
import cats.data.NonEmptyList

import scala.util.Random
import cats.implicits._
import com.petuxbot.domain.Rank.{Ace, Jack, King, Queen, Ten}
import com.petuxbot.domain.Suit.{Clubs, Diamonds, Hearts, Spades}

final case class Deck(cards: List[Card], trumpCard: Card) extends CardContainer {
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
  def make: Deck = {
    val allCards = Rank.values
      .flatMap(rank =>
        Suit.values.map(suit =>
          Card(rank, suit)
        )
      ).toList

//    val ranks = NonEmptyList(Ace, List(King, Queen, Jack, Ten))
//    val suits = NonEmptyList(Clubs, List(Diamonds, Hearts, Spades))
//
//    val allCards: NonEmptyList[Card] = ranks
//      .flatMap(rank =>
//        suits.map(suit =>
//          Card(rank, suit)
//        )
//      )

    val shuffledCards = Random.shuffle(allCards)
    val trumpCard = shuffledCards.last.copy(isTrump = true)


    val shuffledCardsWithTrumps =
      shuffledCards.map(card =>
        if (card.suit == trumpCard.suit) card.copy(isTrump = true) else card
      )

    Deck(shuffledCardsWithTrumps, trumpCard)
  }
}