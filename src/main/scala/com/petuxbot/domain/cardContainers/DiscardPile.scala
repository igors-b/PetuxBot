package com.petuxbot.domain.cardContainers

import com.petuxbot.domain.Card

final case class DiscardPile (cards: List[Card]) extends CardContainer {

  override def addCard(card: Card): DiscardPile = addCards(List(card))

  def addCards(cardsToAdd: List[Card]): DiscardPile = this.copy(cards ++ cardsToAdd)

  override def removeCard(card: Card): DiscardPile = removeCards(List(card))

  def removeCards(cardsToRemove: List[Card]): DiscardPile = this.copy(cards.diff(cardsToRemove))
}

object DiscardPile {
  lazy val Empty: DiscardPile = DiscardPile(List.empty[Card])
}
