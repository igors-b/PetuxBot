package com.petuxbot.domain.cardcontainers

import com.petuxbot.domain.Card

final case class Hand(cards: List[Card]) {

  def addCards(cardsToAdd: List[Card]): Hand = copy(cards ++ cardsToAdd)

  def removeCard(card: Card): Hand = removeCards(List(card))

  def removeCards(cardsToRemove: List[Card]): Hand = copy(cards.diff(cardsToRemove))
}

object  Hand {
  val InitialNumberOfCards = 5
  lazy val Empty: Hand = Hand(List.empty[Card])
}