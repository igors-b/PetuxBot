package com.petuxbot

import io.circe.parser._
import com.petuxbot.ImplicitCodecs._
import com.petuxbot.GameError._

//trait Parser[F[_]] {
//  def parse(value: String): F[Either[Error, Command]]
//}
//
////object Parser {
////
////  def apply[F[_]: Sync]: Parser[F] = new Parser[F] {
////    def parse(value: String): F[Either[Error, Command]] =
////      Sync[F].delay {
////        decode[Command](value.stripMargin)
////      }
////  }
////}

object Parser {

  def parse(cmd: String): Either[GameError, Command] =
    decode[Command](cmd.stripMargin) match {
      case Left(_)        => Left(ParsingError)
      case Right(command) => Right(command)
    }
}
