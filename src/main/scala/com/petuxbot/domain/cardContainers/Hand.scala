package com.petuxbot.domain.cardContainers

import com.petuxbot.domain.Card

final case class Hand(cards: List[Card]) extends CardContainer {

  def addCard(card: Card): Hand = this.copy(cards :+ card)

  def addCards(cardsToAdd: List[Card]): Hand = this.copy(cards ++ cardsToAdd)

  def removeCard(card: Card): Hand = this.copy(cards.filterNot(_ == card))

  def removeCards(cardsToRemove: List[Card]): Hand = this.copy(cards.diff(cardsToRemove))
}

object  Hand {
  val InitialNumberOfCards = 5
  lazy val Empty= Hand(List.empty[Card])
}