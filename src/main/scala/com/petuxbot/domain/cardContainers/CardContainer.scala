package com.petuxbot.domain.cardContainers

import com.petuxbot.domain.Card
trait CardContainer {

  def addCard(card: Card): CardContainer

  def addCards(cardsToAdd: List[Card]): CardContainer

  def removeCard(card: Card): CardContainer

  def removeCards(card: Card): CardContainer
}


