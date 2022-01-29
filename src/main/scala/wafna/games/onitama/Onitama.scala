package wafna.games.onitama

import wafna.util.Player
import wafna.util.Player.{P1, P2}

import scala.util.chaining._

case class Hand(c1: Card, c2: Card) {
  def cycle(out: Card, in: Card): Hand = {
    if (c1 == out) { Hand(in, c2) }
    else if (c2 == out) { Hand(c1, in) }
    else {
      throw new IllegalArgumentException(s"Card $c1 not in hand $toList")
    }
  }
  def toList: List[Card] = List(c1, c2)
}

sealed trait Kind
case object Pawn extends Kind
case object King extends Kind

case class Piece(owner: Player, kind: Kind)

// The direction of the pass indicates the player with turn in hand.
class Onitama private(val p1: Hand, val p2: Hand, val pass: Either[Card, Card], val board: Board) {

  def grid(): Iterator[Option[Piece]] = board.grid

  def moves(): List[Onitama] = {

    def moveP1(hand: Hand, passCard: Card, board: Board) =
      new Onitama(hand, p2, Left(passCard), board)

    def moveP2(hand: Hand, passCard: Card, board: Board) =
      new Onitama(p1, hand, Right(passCard), board)

    val (player, hand, passCard, mover) = pass match {
      case Right(c) => (P1, p1, c, moveP1 _)
      case Left(c)  => (P2, p2, c, moveP2 _)
    }

    val pawns = board.occupied(player)
    hand.toList.flatMap { card =>
      val hand1 = hand.cycle(card, passCard)
      val moves = (for {
        pawn <- pawns
        move <- card.moves
      } yield board.move(pawn, move)).flatten
      moves.map { move =>
        mover(hand1, card, move)
      }
    }
  }
}

object Onitama {

  def apply(): Onitama = Deck() match {
    case (p1, p2, pass) => new Onitama(p1, p2, Right(pass), Board())
  }
}
