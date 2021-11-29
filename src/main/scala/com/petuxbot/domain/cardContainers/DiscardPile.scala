package com.petuxbot.domain.cardContainers

import com.petuxbot.domain.Card

final case class DiscardPile (cards: List[Card]) extends CardContainer {

  def addCard(card: Card): DiscardPile = this.copy(cards :+ card)

  def addCards(cardsToAdd: List[Card]): DiscardPile = this.copy(cards ++ cardsToAdd)

  def removeCard(card: Card): DiscardPile = this.copy(cards.filterNot(_ == card))

  def removeCards(card: Card): DiscardPile = this.copy(cards.filterNot(_ == card))
}

object DiscardPile {
  lazy val Empty: DiscardPile = DiscardPile(List.empty[Card])
}
