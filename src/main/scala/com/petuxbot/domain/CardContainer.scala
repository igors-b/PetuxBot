package com.petuxbot.domain

trait CardContainer {
  def cards: List[Card]

  def addCard (card: Card): CardContainer

  def removeCard (card: Card): CardContainer
}