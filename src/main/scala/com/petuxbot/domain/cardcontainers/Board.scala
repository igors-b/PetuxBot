package com.petuxbot.domain.cardcontainers

import com.petuxbot.domain.{Card, StrongestCard}

final case class Board(cards: List[Card], cardToHit: Option[Card], strongestCard: Option[StrongestCard]) {

  def addCard(card: Card): Board = this.copy(cards :+ card)

  def setCardToHit(card: Card): Board = this.copy(cardToHit = Some(card))

  def setStrongestCard(card: Card, ownerId: Long): Board = this.copy(strongestCard = Some(StrongestCard(card, ownerId)))
}

object Board {
  lazy val Empty: Board = Board(cards = List.empty[Card], cardToHit = None, strongestCard = None)
}
