package com.petuxbot

import com.petuxbot.domain.{Card, Player, Score, StrongestCard}

object CardValidator {
  def isCardValidToMakeTurn(card: Card, player: Player, scores: List[Score]): Boolean =
    if (player.hasCard(card))
      scores.exists(_.value <=3) && card.isTrump || !scores.exists(_.value <=3)
    else false

  def isCardValidToMakeAttack(card: Card, player: Player, /*state: GameState*/ strongestCard: StrongestCard, cardToHit: Card): Boolean = {
    val cards = player.cards
    //val cardToHitOpt = state.board.cardToHit
    //val strongestCardOpt = state.board.strongestCard
    val trumpCards = cards.filter(_.isTrump)
//    for {
//      cardToHit     <- cardToHitOpt
//      strongestCard <- strongestCardOpt
      val cardsOfRequiredSuit =
      cards.filter(_.suit == cardToHit.suit)

      val cardsOfRequiredSuitAndStrongerThanStrongestCard =
        cardsOfRequiredSuit
        .filter(_.strength > strongestCard.value.strength)

      val res =
        if (player.hasCard(card)) {
          if (!cardToHit.isTrump) {
            if (player.hasCardOfSuit(cardToHit.suit)) {
              if (strongestCard.value.suit == cardToHit.suit) {
                if (cardsOfRequiredSuitAndStrongerThanStrongestCard.nonEmpty)
                  cardsOfRequiredSuitAndStrongerThanStrongestCard.contains(card)
                else cardsOfRequiredSuit.contains(card)
              }
              else cardsOfRequiredSuit.contains(card)
            }
            else if (trumpCards.nonEmpty) {
              if(strongestCard.value.isTrump) {
                val trumpCardsStrongerThenStrongestCard =
                  trumpCards.filter(_.strength > strongestCard.value.strength)
                if (trumpCardsStrongerThenStrongestCard.nonEmpty)
                  trumpCardsStrongerThenStrongestCard.contains(card)
                else trumpCards.contains(card)
              }
              else trumpCards.contains(card)
            }
            else true
          }
          else if (trumpCards.nonEmpty) {
            val trumpCardsStrongerThenStrongestCard =
              trumpCards.filter(_.strength > strongestCard.value.strength)
            if (trumpCardsStrongerThenStrongestCard.nonEmpty)
              trumpCardsStrongerThenStrongestCard.contains(card)
            else trumpCards.contains(card)
          }
          else true
        }
        else false
//    } yield res


res
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
