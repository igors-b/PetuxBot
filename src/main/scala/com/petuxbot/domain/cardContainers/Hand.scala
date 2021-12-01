package com.petuxbot.domain.cardContainers

import com.petuxbot.domain.Card

final case class Hand(cards: List[Card]) extends CardContainer {

  def addCards(cardsToAdd: List[Card]): Hand = this.copy(cards ++ cardsToAdd)

  def removeCards(cardsToRemove: List[Card]): Hand = this.copy(cards.diff(cardsToRemove))
}

object  Hand {
  val InitialNumberOfCards = 5
  lazy val Empty: Hand = Hand(List.empty[Card])
}