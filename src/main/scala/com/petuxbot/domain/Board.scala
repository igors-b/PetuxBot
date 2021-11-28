package com.petuxbot.domain

final case class Board(cards: List[Card], cardToHit: Option[Card], strongestCard: Option[Card]) {
  def addCard(card: Card): Board = this.copy(cards :+ card)

}

object Board {
  lazy val Empty = Board(cards = List.empty[Card], cardToHit = None, strongestCard = None)
}