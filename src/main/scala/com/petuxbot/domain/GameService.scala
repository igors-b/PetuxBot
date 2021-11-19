package com.petuxbot.domain

import cats.effect.Sync
import com.petuxbot.Response

case class GameService[F[_] : Sync](gameState: GameState[F]) {
//  def update(newGameState: GameState[F]) = this.copy[F](newGameState)
//
//  def startGame(players: Vector[Player]) =
//    for {
//      gameState <- gameState.startGame(players)
//    } yield (GameService(gameState), Response)
//}
//
//object GameService {
//  def empty[F[_] : Sync]: F[GameService[F]] = {
//      for {
//        gameState <- GameState.make
//      } yield GameService(gameState)
//  }
}
