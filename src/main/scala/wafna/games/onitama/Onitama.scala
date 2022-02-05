package wafna.games
package onitama

import cats.data.NonEmptyList
import wafna.games.minmax.{GameOver, Win}
import wafna.games.Player
import wafna.games.Player.{P1, P2}

import scala.util.Random
import scala.util.chaining.*

case class Hand(c1: Card, c2: Card) {
  def cycle(out: Card, in: Card): Hand = {
    if (c1 == out) { Hand(in, c2) }
    else if (c2 == out) { Hand(c1, in) }
    else {
      throw new IllegalArgumentException(s"Card $c1 not in hand $toNel")
    }
  }
  def toNel: NonEmptyList[Card] = nonEmptyList(c1, c2)
}

sealed trait Kind
case object Pawn extends Kind
case object King extends Kind

case class Piece(owner: Player, kind: Kind)

// The direction of the pass indicates the player with turn in hand.
class Onitama private (val p1: Hand, val p2: Hand, val pass: Either[Card, Card], val board: Board) {

  def grid(): Iterator[Option[Piece]] = board.grid

  @throws[RuntimeException]("If the game is over, so don't ask.")
  def moves(): NonEmptyList[Onitama] = {

    gameOver.foreach { end =>
      sys.error(s"No moves; game is over: $end")
    }

    def moveP1(hand: Hand, passCard: Card, board: Board) =
      new Onitama(hand, p2, Left(passCard), board)

    def moveP2(hand: Hand, passCard: Card, board: Board) =
      new Onitama(p1, hand, Right(passCard), board)

    val (player, hand, passCard, mover) = pass match {
      case Right(c) => (P1, p1, c, moveP1 _)
      case Left(c)  => (P2, p2, c, moveP2 _)
    }

    val pawns = board.occupied(player)
    hand.toNel.flatMap { card =>
      val hand1 = hand.cycle(card, passCard)
      val moves = (for {
        pawn <- pawns
        move <- card.moves.iterator
      } yield board.move(pawn, move)).flatten
      moves.map { move =>
        mover(hand1, card, move)
      }.toList match {
        case Nil =>
          ???
        case head :: tail => NonEmptyList(head, tail)
      }
    }
  }

  //noinspection ScalaStyle
  val gameOver: Option[GameOver] = {
    // Get each player's pieces.
    val pieces: (List[Piece], List[Piece]) =
      grid().foldLeft((List.empty[Piece], List.empty[Piece])) { (pieces, spot) =>
        spot match {
          case None => pieces
          case Some(piece) =>
            piece.owner match {
              case P1 => (piece :: pieces._1, pieces._2)
              case P2 => (pieces._1, piece :: pieces._2)
            }
        }
      }
    // Player is absent from the field.
    if (pieces._1.isEmpty) {
      Some(Win(P2))
    } else if (pieces._2.isEmpty) {
      Some(Win(P1))
    } else {
      // Player has lost their King.
      if (!pieces._1.exists(_.kind == King)) {
        Some(Win(P2))
      } else if (!pieces._2.exists(_.kind == King)) {
        Some(Win(P1))
      } else {
        // Player has usurped the throne.
        if (board.spot(Spot(2, 4)).contains(Piece(P1, King))) {
          Some(Win(P1))
        } else if (board.spot(Spot(2, 0)).contains(Piece(P2, King))) {
          Some(Win(P1))
        } else {
          // Play on!
          None
        }
      }
    }
  }
}

object Onitama {

  def apply()(implicit random: Random = Random): Onitama = Deck() match {
    case (p1, p2, pass) => new Onitama(p1, p2, Right(pass), Board())
  }
  def apply(hand1: Hand, hand2: Hand, pass: Card): Onitama =
    new Onitama(hand1, hand2, Right(pass), Board())
}
