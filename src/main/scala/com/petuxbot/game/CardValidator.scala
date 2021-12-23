package com.petuxbot.game

import com.petuxbot.domain.{Card, Player, Score, StrongestCard}

object CardValidator {
  def isCardValidToMakeTurn(card: Card, player: Player, scores: List[Score]): Boolean =
    if (player.hasCard(card)) {
      if (scores.exists(_.value <= 3) && player.hand.cards.exists(_.isTrump)) card.isTrump
      else true
    }
    else false

  def isCardValidToMakeAttack(
    card: Card,
    player: Player, strongestCard: StrongestCard,
    cardToHit: Card
  ): Boolean = {
    val cards = player.cards

    def trumpCards = cards.filter(_.isTrump)

    def cardsOfRequiredSuit =
      cards.filter(_.suit == cardToHit.suit)

    def cardsOfRequiredSuitAndStrongerThanStrongestCard =
      cardsOfRequiredSuit
        .filter(_.strength > strongestCard.value.strength)

    def trumpCardsStrongerThenStrongestCard =
      trumpCards.filter(_.strength > strongestCard.value.strength)
//
    def cardToHitIsNotTrump =
      if (player.hasCardOfSuit(cardToHit.suit)) playerHasCardOfSuit
      else if (trumpCards.nonEmpty) {
        cardToHitIsNotTrumpAndTrumpCardsNonEmpty}
      else true

    def playerHasCardOfSuit =
      if (strongestCard.value.suit == cardToHit.suit && cardsOfRequiredSuitAndStrongerThanStrongestCard.nonEmpty)
        cardsOfRequiredSuitAndStrongerThanStrongestCard.contains(card)
      else cardsOfRequiredSuit.contains(card)

    def cardToHitIsNotTrumpAndTrumpCardsNonEmpty =
      if (strongestCard.value.isTrump && trumpCardsStrongerThenStrongestCard.nonEmpty)
        trumpCardsStrongerThenStrongestCard.contains(card)
      else trumpCards.contains(card)

    def trumpCardsNonEmpty =
      if (trumpCardsStrongerThenStrongestCard.nonEmpty) trumpCardsStrongerThenStrongestCard.contains(card)
      else trumpCards.contains(card)

    val result =
      if (player.hasCard(card))
        if (!cardToHit.isTrump) cardToHitIsNotTrump
        else if (trumpCards.nonEmpty) trumpCardsNonEmpty
        else true
      else false

    result
  }

  def isCardStronger(card: Card, strongestCard: Card): Boolean = {
    if (strongestCard.isTrump) {
      if (card.isTrump) card.strength > strongestCard.strength
      else false
    }
    else if (card.isTrump) true
    else if (card.suit == strongestCard.suit) card.strength > strongestCard.strength
    else false
  }
}
