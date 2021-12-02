package com.petuxbot.domain.cardContainers

import com.petuxbot.domain.Card

final case class Trick(cards: List[Card]) extends CardContainer {

  def addCard(card: Card): CardContainer = addCards(List(card))

  def addCards(cardsToAdd: List[Card]): Trick = this.copy(cards ++ cardsToAdd)

  def removeCard(card: Card): CardContainer = removeCards(List(card))

  def removeCards(cardsToRemove: List[Card]): Trick = this.copy(cards.diff(cardsToRemove))
}
