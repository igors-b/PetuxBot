package com.petuxbot.domain.cardcontainers

import com.petuxbot.domain.{Card, StrongestCard}

final case class Board(cards: List[Card], cardToHit: Option[Card], strongestCard: Option[StrongestCard]) {

  def addCard(card: Card): Board = addCards(List(card))

  def addCards(cardsToAdd: List[Card]): Board = this.copy(cards ++ cardsToAdd)

  def removeCard(card: Card): Board = removeCards(List(card))

  def removeCards(cardsToRemove: List[Card]): Board = this.copy(cards.diff(cardsToRemove))

  def setCardToHit(card: Card): Board = this.copy(cardToHit = Some(card))

  def setStrongestCard(card: Card, ownerId: Long): Board = this.copy(strongestCard = Some(StrongestCard(card, ownerId)))
}

object Board {
  lazy val Empty: Board = Board(cards = List.empty[Card], cardToHit = None, strongestCard = None)

  Board(List.empty, None, None)
}
