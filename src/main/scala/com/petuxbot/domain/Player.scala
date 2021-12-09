package com.petuxbot.domain

import com.petuxbot.domain.cardContainers.{Hand, Trick}

case class Player(id: Long, name: String, hand: Hand, score: Score, tricks: List[Trick]) {
  def numberOfTricks: Int = tricks.size

  def addCardsToHand(cards: List[Card]): Player =
    this.copy(hand = hand.addCards(cards))

  def removeCardsFromHand(cards: List[Card]): Player = this.copy(hand = hand.removeCards(cards))

  def hasCard(card: Card): Boolean = this.hand.cards.contains(card)

  def hasCardOfSuit(suit: Suit): Boolean = this.hand.cards.map(_.suit).contains(suit)

  val cards = this.hand.cards

}