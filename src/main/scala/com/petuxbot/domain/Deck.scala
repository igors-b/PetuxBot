package com.petuxbot.domain
import scala.util.Random
import cats.implicits._

final case class Deck(cards: Vector[Card], trumpCard: Card) extends CardContainer {
  override def addCard(card: Card): Deck = this.copy(cards :+ card)

  override protected def removeCard(card: Card): Deck =
    this.copy(cards filterNot (c => c == card))

   def deal(hands: Vector[Hand]): Option[(Deck, Vector[Hand])] =
   {
     if (hands.nonEmpty) {
       if (cards.nonEmpty) {
         hands.foldLeft((Vector.empty[Hand], cards)) {
           case ((hands, cards), hand) =>
             val (taken, rest) = cards.splitAt(Hand.initialNumberOfCards - hand.cards.size)
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
      ).toVector

    val shuffledCards = Random.shuffle(allCards)
    val trumpCard = shuffledCards.last //throws NoSuchElementException

    val shuffledCardsWithTrumps =
      shuffledCards.map(card =>
        if (card.suit == trumpCard.suit) card.copy(isTrump = true) else card
      )

    Deck(shuffledCardsWithTrumps, trumpCard)
  }
}