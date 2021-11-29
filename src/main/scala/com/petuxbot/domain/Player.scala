package com.petuxbot.domain

import com.petuxbot.domain.cardContainers.{Hand, Trick}

case class Player(name: String, hand: Hand, score: Score, tricks: List[Trick]) {
  def numberOfTricks: Int = tricks.size

  def addCardsToHand(cards: List[Card]) =
    this.copy(hand = hand.addCards(cards))

  //UserID ???
  // dealer ???

}