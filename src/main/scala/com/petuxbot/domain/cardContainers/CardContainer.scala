package com.petuxbot.domain.cardContainers

import com.petuxbot.domain.Card
trait CardContainer {

  def addCard(card: Card): CardContainer

  def addCards(cardsToAdd: List[Card]): CardContainer

  def removeCard(card: Card): CardContainer

  def removeCards(cards: List[Card]): CardContainer
}

//case class CardContainer(cards: List[Card]) extends Container{
//  def addCard(card: Card): CardContainer = this.copy(cards :+ card)
//
//  def addCards(cardsToAdd: List[Card]): CardContainer = this.copy(cards ++ cardsToAdd)
//
//  def removeCard(card: Card): CardContainer = this.copy(cards.filterNot(_ == card))
//
//  def removeCards(card: Card): CardContainer = this.copy(cards.filterNot(_ == card))
//}




