package com.petuxbot.domain

import com.petuxbot.domain.cardcontainers.{Hand, Trick}

case class Player(id: Long, name: String, hand: Hand, score: Score, tricks: List[Trick]) {

  val cards: List[Card] = hand.cards

  def numberOfTricks: Int = tricks.size

  def addCardsToHand(cards: List[Card]): Player =
    this.copy(hand = hand.addCards(cards))

  def removeCardsFromHand(cardsToRemove: List[Card]): Option[Player] = {
    val checkList = cardsToRemove.map(card => cards.contains(card))
    if (checkList.contains(false)) None
    else Some(this.copy(hand = hand.removeCards(cardsToRemove)))
  }

  def hasCard(card: Card): Boolean = hand.cards.contains(card)

  def hasCardOfSuit(suit: Suit): Boolean = hand.cards.map(_.suit).contains(suit)
}
