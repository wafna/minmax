package wafna.games.onitama

import wafna.games.util.Player
import wafna.games.util.Player.{P1, P2}

import scala.collection.immutable.ArraySeq

final case class Spot(x: Int, y: Int) {
  def valid: Boolean = 0 <= x && x < 5 && 0 <= y && y < 5
  def +(move: Move): Spot = Spot(x + move.x, y + move.y)
  def toIx: Int = x + (5 * y)
}

object Spot {
  def fromIx(ix: Int): Spot = Spot(ix % 5, ix / 5)
}

class Board private (spots: ArraySeq[Option[Piece]]) {
  var grid: Iterator[Option[Piece]] = spots.iterator

  require(25 == spots.length, s"Required 25 spots, got ${spots.length}")

  @inline def spot(spot: Spot): Option[Piece] = spots(spot.toIx)

  def move(from: Spot, move: Move): Option[Board] = {
    val source = spot(from).getOrElse(throw new IllegalArgumentException(s"No piece at spot $from"))
    val target = from + move
    if (!target.valid) {
      None
    } else if (spot(target).exists(_.owner == source.owner)) {
      None
    } else {
      Some(new Board(spots.updated(target.toIx, Some(source)).updated(from.toIx, None)))
    }
  }

  def occupied(player: Player): IndexedSeq[Spot] =
    (0 until 25).filter(ix => spots(ix).exists(_.owner == player)).map(Spot.fromIx)
}

object Board {
  def apply(): Board = new Board(
    ArraySeq(
      Some(Piece(P1, Pawn)),
      Some(Piece(P1, Pawn)),
      Some(Piece(P1, King)),
      Some(Piece(P1, Pawn)),
      Some(Piece(P1, Pawn)),
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      Some(Piece(P2, Pawn)),
      Some(Piece(P2, Pawn)),
      Some(Piece(P2, King)),
      Some(Piece(P2, Pawn)),
      Some(Piece(P2, Pawn))
    )
  )
}
