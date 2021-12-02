package com.petuxbot.domain.cardContainers

import com.petuxbot.domain.Card
trait CardContainer {

  def addCard(card: Card): CardContainer

  def addCards(cards: List[Card]): CardContainer

  def removeCard(card: Card): CardContainer

  def removeCards(cards: List[Card]): CardContainer
}




