package com.petuxbot

import cats.effect.Sync
import com.petuxbot.domain._
import com.petuxbot.Response._
import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Decoder, Encoder}
import cats.syntax.functor._

trait Command
case object Command {
  object StartGame extends Command
  object WrongCommand extends Command
  object DealCard extends Command
}

object CommandProcessor {
  def process[F[_]: Sync](gameState: GameState, command: Command): F[(GameState, Response)] = {

    command match {
      case Command.DealCard => ???
      case Command.StartGame => GameState.startGame(gameState.players).map {
        case Some(newState) => (newState, OK)
        case None => (gameState, Error("Dealing cards failed"))
      }
      case _ => Sync[F].delay(gameState, Error("You entered wrong command"))
    }
  }
}

object ImplicitCodecs {


  implicit val suitDecoder: Decoder[Suit] =
    Decoder.decodeString.emap(str =>
    Suit.values.find(_.toString == str).toRight("invalid suit")
  )

  implicit val suitEncoder: Encoder[Suit] =
    Encoder.encodeString.contramap(_.toString)

  implicit val rankDecoder: Decoder[Rank] = Decoder.decodeString.emap(string =>
    Rank.values.find(_.toString == string).toRight("invalid rank")
  )

  implicit val rankEncoder: Encoder[Rank] =
    Encoder.encodeString.contramap(_.toString)

  implicit val cardCodec: Codec[Card] = deriveCodec[Card]

  implicit val deckCodec: Codec[Deck] = deriveCodec[Deck]

  implicit val handCodec: Codec[Hand] = deriveCodec[Hand]

  implicit val boardCodec: Codec[Board] = deriveCodec[Board]

  //implicit val errorCodec: Codec[Error] = deriveCodec[Error]


  implicit val PlayerHandStatusCodec: Codec[Response] = deriveCodec[Response]
//  implicit val responseEncoder: Encoder[Response] = Encoder.instance {
//    case ok @ OK => ok.asJson
//    case error @ Error(_) => error.asJson
//  }
//
//  implicit val responseDecoder: Decoder[Response] =
//    List[Decoder[Response]](
//      Decoder[OK].widen,
//      Decoder[Error].widen,
//    ).reduceLeft(_ or _)

//  implicit val decodeMode: Decoder[Response] = Decoder[String].emap {
//    case "OK"  => Right(OK)
//    case other => Left(s"invalid response")
//  }
//
//  implicit val encodeMode: Encoder[Mode] = Encoder[String].contramap {
//    case OK => "OK"
//  }

}
