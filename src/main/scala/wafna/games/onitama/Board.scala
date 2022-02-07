package wafna.games
package onitama

import scala.collection.immutable.ArraySeq

final case class Spot(x: Int, y: Int) {
  def valid: Boolean = 0 <= x && x < 5 && 0 <= y && y < 5
  def +(move: Move): Spot = Spot(x + move.x, y + move.y)
  def toIx: Int = x + (5 * y)
}

object Spot {
  def fromIx(ix: Int): Spot = Spot(ix % 5, ix / 5)
}

sealed trait Kind
case object Pawn extends Kind
case object King extends Kind

case class Piece(owner: Player, kind: Kind)

class Board private[onitama] (val spots: ArraySeq[Option[Piece]]) {

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

  //noinspection ScalaStyle
  val gameOver: Option[GameOver] = {
    // Get each player's pieces.
    val pieces: (List[Piece], List[Piece]) =
      spots.foldLeft((List.empty[Piece], List.empty[Piece])) { (pieces, spot) =>
        spot match {
          case None => pieces
          case Some(piece) =>
            piece.owner match {
              case P1 => (piece :: pieces._1, pieces._2)
              case P2 => (pieces._1, piece :: pieces._2)
            }
        }
      }
    // Player has lost their King.
    if (!pieces._1.exists(_.kind == King)) {
      Some(Win(P2))
    } else if (!pieces._2.exists(_.kind == King)) {
      Some(Win(P1))
    } else {
      // Player has usurped the throne.
      if (spot(Spot(2, 4)).contains(Piece(P1, King))) {
        Some(Win(P1))
      } else if (spot(Spot(2, 0)).contains(Piece(P2, King))) {
        Some(Win(P2))
      } else {
        // Play on!
        None
      }
    }
  }
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
