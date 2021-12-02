package com.petuxbot.domain

import com.petuxbot.domain.cardContainers.{Hand, Trick}

case class Player(id: Long, name: String, hand: Hand, score: Score, tricks: List[Trick]) {
  def numberOfTricks: Int = tricks.size

  def addCardsToHand(cards: List[Card]): Player =
    this.copy(hand = hand.addCards(cards))

  def removeCardsFromHand(cards: List[Card]): Player = this.copy(hand = hand.removeCards(cards))

  //UserID ???
  // dealer ???

}