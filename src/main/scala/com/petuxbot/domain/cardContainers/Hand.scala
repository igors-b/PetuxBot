package com.petuxbot.domain.cardContainers

import com.petuxbot.domain.Card

final case class Hand(cards: List[Card]) extends CardContainer {

  def addCard(card: Card): Hand = addCards(List(card))

  def addCards(cardsToAdd: List[Card]): Hand = this.copy(cards ++ cardsToAdd)

  def removeCard(card: Card): Hand = removeCards(List(card))

  def removeCards(cardsToRemove: List[Card]): Hand = this.copy(cards.diff(cardsToRemove))
}

object  Hand {
  val InitialNumberOfCards = 5
  lazy val Empty: Hand = Hand(List.empty[Card])
}