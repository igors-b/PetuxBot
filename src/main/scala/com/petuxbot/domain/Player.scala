package com.petuxbot.domain

case class Player(name: String, hand: Hand, score: Score, tricks: Vector[Trick]) {
  def numberOfTricks: Int = tricks.size

  def addCardsToHand(cards: Vector[Card]) =
    this.copy(hand = hand.addCards(cards))

  //UserID ???
  // dealer ???
}