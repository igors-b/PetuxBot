package com.petuxbot.domain

final case class DiscardPile (cards: List[Card]) extends CardContainer {
  def addCard(card: Card): DiscardPile =
    this.copy(cards = cards :+ card)

  def addCards(newCards: List[Card]): DiscardPile =
    this.copy(cards = cards ++ newCards)

  def removeCard(card: Card): DiscardPile = ???

}

object DiscardPile {
  lazy val Empty: DiscardPile = DiscardPile(List.empty[Card])
}
