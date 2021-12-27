# PetuxBot

Course project for [Evolution Bootcamp[Scala] - 2021](https://github.com/evolution-gaming/scala-bootcamp). 

## Description

PetuxBot is a Telegram bot application written in Scala which represents a card game Petux.
Where a Telegram user plays Petux game with a bot in single player mode. 

## Petux card game rules

* Number of decks: 1
* Number of cards in deck: 20 - 36 (depending on number of players)
* Number of players: 2 - 5
* Number of cards in player’s hand: 5
* The main goal of the game: to write off 15 points first
* At the beginning of the game each player has 15 points of score
* Each point writes off by each trick taken during the round
* At the beginning of each game round each player is dealt five cards, 
the last card in the deck is the trump card
* Then each player may discard any number of cards he’s got and get back 
(from the deck) as many cards as player discarded
* Each game round starts with a player to the left from  the dealer.
Next turn starts a player who wins current drawing and takes a trick
* Player can start turn with any card. If anybody has less than 4 points of score, 
player has to start turn with a trump
* Players has to attack with a card of the same suit as start card 
but of the highest rank or to beat it with a trump
* Player who does not take any trick during the round gets penalty of +5 points of score 
by adding them to his points

## Run

Start scala backend:

```shell
sbt run
```

Start client:

1. Run Telegram application
2. In Telegram search log type "PetuxBot"
3. Find PetuxBot and start new chat with it
4. Follow game instructions provided during the game

