package com.petuxbot.domain.cardcontainers

import com.petuxbot.domain.Card

final case class Trick(cards: List[Card]) {

  def addCard(card: Card): Trick = addCards(List(card))

  def addCards(cardsToAdd: List[Card]): Trick = this.copy(cards ++ cardsToAdd)

  def removeCard(card: Card): Trick = removeCards(List(card))

  def removeCards(cardsToRemove: List[Card]): Trick = this.copy(cards.diff(cardsToRemove))
}
