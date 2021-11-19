package com.petuxbot.domain

case class Hand(cards: Vector[Card]) {

  def addCards(cardsToAdd: Vector[Card]): Hand = this.copy(cards ++ cardsToAdd)

  protected def removeCard(card: Card): Hand =
    this.copy(cards filterNot (c => c == card))
}

object  Hand {
  val initialNumberOfCards = 5
  def empty = Hand(Vector.empty[Card])
}