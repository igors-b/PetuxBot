package com.petuxbot.domain.cardContainers

import com.petuxbot.domain.Card

final case class Trick(cards: List[Card]) extends CardContainer {

  def addCards(cardsToAdd: List[Card]): Trick = this.copy(cards ++ cardsToAdd)

  def removeCards(cardsToRemove: List[Card]): Trick = this.copy(cards.diff(cardsToRemove))
}
