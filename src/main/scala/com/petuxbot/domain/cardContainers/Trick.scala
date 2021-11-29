package com.petuxbot.domain.cardContainers

import com.petuxbot.domain.Card

final case class Trick(cards: List[Card]) extends CardContainer {
  def addCard(card: Card): Trick = this.copy(cards :+ card)

  def addCards(cardsToAdd: List[Card]): Trick = this.copy(cards ++ cardsToAdd)

  def removeCard(card: Card): Trick = this.copy(cards.filterNot(_ == card))

  def removeCards(card: Card): Trick = this.copy(cards.filterNot(_ == card))
}
