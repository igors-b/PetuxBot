package com.petuxbot.domain

trait CardContainer {
  val cards: Vector[Card]

  def addCard (card: Card): CardContainer

  protected def removeCard (card: Card): CardContainer
}