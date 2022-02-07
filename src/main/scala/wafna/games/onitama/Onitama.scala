package wafna.games
package onitama

import cats.data.NonEmptyList

import scala.util.Random
import scala.util.chaining.*

case class Hand(c1: Card, c2: Card) {

  @throws[IllegalArgumentException]("If the out card is not in hand.")
  def cycle(out: Card, in: Card): Hand = {
    if (c1 == out) { Hand(in, c2) }
    else if (c2 == out) { Hand(c1, in) }
    else {
      throw new IllegalArgumentException(s"Card $c1 not in hand $toNel")
    }
  }
  def toNel: NonEmptyList[Card] = nonEmptyList(c1, c2)
}

sealed abstract class TurnInHand {
  def card: Card
  def player: Player
  def choose[T](p1: T, p2: T): T = player match {
    case P1 => p1
    case P2 => p2
  }
}
final case class TurnP1(card: Card) extends TurnInHand {
  def player: Player = P1
}
final case class TurnP2(card: Card) extends TurnInHand {
  def player: Player = P2
}

class Onitama private[onitama] (val p1: Hand, val p2: Hand, val turnInHand: TurnInHand, val board: Board) {

  val currentPlayer: Player = turnInHand.player

  val gameOver: Option[GameOver] = board.gameOver

  @throws[RuntimeException]("If the game is over, so don't ask.")
  def moves(): NonEmptyList[Onitama] = {

    board.gameOver.foreach { end =>
      sys.error(s"No moves; game is over: $end")
    }

    def moveP1(hand: Hand, passCard: Card, board: Board) =
      new Onitama(hand, p2, TurnP2(passCard), board)

    def moveP2(hand: Hand, passCard: Card, board: Board) =
      new Onitama(p1, hand, TurnP1(passCard), board)

    val (player, hand, passCard, mover, flip) = turnInHand match {
      case TurnP1(c) => (P1, p1, c, moveP1 _, identity[Move])
      case TurnP2(c)  => (P2, p2, c, moveP2 _, (move: Move) => move.flip)
    }

    val pieces = board.occupied(player)

    hand.toNel.flatMap { card =>
      val handMoveSelected = hand.cycle(card, passCard)

      val allMovesForCard: Seq[Onitama] = {
        (for {
          pawn <- pieces
          move <- card.moves.iterator
        } yield board.move(pawn, flip(move))).flatten
          .map { board =>
            mover(handMoveSelected, card, board)
          }
      }

      NonEmptyList
        .fromFoldable(allMovesForCard)
        .getOrElse {
          // In this manner, each card in the hand becomes a move in its own right,
          // not affecting the state of the board, though.
          // This may lead to infinite sequences of passing. Hmm.
          NonEmptyList.one(mover(handMoveSelected, card, board))
        }
    }
  }

}

object Onitama {

  def apply()(implicit random: Random = Random): Onitama = Deck() match {
    case (p1, p2, pass) => new Onitama(p1, p2, TurnP1(pass), Board())
  }
  def apply(hand1: Hand, hand2: Hand, pass: Card): Onitama =
    new Onitama(hand1, hand2, TurnP1(pass), Board())
}
