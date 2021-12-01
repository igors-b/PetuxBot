package com.petuxbot.domain.cardContainers

import com.petuxbot.domain.Card
trait CardContainer {

  def addCard(card: Card): CardContainer = addCards(List(card))

  def addCards(cards: List[Card]): CardContainer

  def removeCard(card: Card): CardContainer = removeCards(List(card))

  def removeCards(cards: List[Card]): CardContainer
}




