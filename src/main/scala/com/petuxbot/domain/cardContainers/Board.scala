package com.petuxbot.domain.cardContainers

import com.petuxbot.domain.Card

final case class Board(cards: List[Card], cardToHit: Option[Card], strongestCard: Option[Card]) extends CardContainer {

  def addCard(card: Card): Board = addCards(List(card))

  def addCards(cardsToAdd: List[Card]): Board = this.copy(cards ++ cardsToAdd)

  def removeCard(card: Card): Board = removeCards(List(card))

  def removeCards(cardsToRemove: List[Card]): Board = this.copy(cards.diff(cardsToRemove))
}

object Board {
  lazy val Empty: Board = Board(cards = List.empty[Card], cardToHit = None, strongestCard = None)

  Board(List.empty, None, None)
}
