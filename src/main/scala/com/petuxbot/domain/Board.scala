package com.petuxbot.domain

case class Board(cards: Vector[Card], cardToHit: Option[Card], strongestCard: Option[Card]) {
  def addCard(card: Card): Board = this.copy(cards :+ card)

}

object Board {
  def empty = Board(cards = Vector.empty[Card], cardToHit = None, strongestCard = None)
}