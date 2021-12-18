package com.petuxbot.domain.cardcontainers

import com.petuxbot.domain.{Card, StrongestCard}

final case class Board(cards: List[Card], cardToHit: Option[Card], strongestCard: Option[StrongestCard]) {

  def addCard(card: Card): Board = copy(cards :+ card)

  def setCardToHit(card: Card): Board = copy(cardToHit = Some(card))

  def setStrongestCard(card: Card, ownerId: Long): Board = copy(strongestCard = Some(StrongestCard(card, ownerId)))
}

object Board {
  lazy val Empty: Board = Board(cards = List.empty[Card], cardToHit = None, strongestCard = None)
}
