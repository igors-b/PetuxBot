package com.petuxbot.domain

final case class Hand(cards: List[Card]) {

  def addCards(cardsToAdd: List[Card]): Hand = this.copy(cards ++ cardsToAdd)

  def removeCard(card: Card): Hand =
    this.copy(cards filterNot (c => c == card))
}

object  Hand {
  val InitialNumberOfCards = 5
  lazy val Empty= Hand(List.empty[Card])
}