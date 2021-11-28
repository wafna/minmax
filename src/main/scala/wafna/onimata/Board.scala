package wafna.onimata

import scala.collection.immutable.ArraySeq

class Board private (spots: ArraySeq[Option[Piece]]) {
  require(25 == spots.length)

  def spot(spot: Spot): Option[Piece] = spots(spot.x + (5 * spot.y))

  def move(from: Spot, delta: Spot): Option[Board] = {
    val source = spot(from).getOrElse(throw new IllegalArgumentException(s"No piece at spot $from"))
    val target = from + delta
    if (!target.valid) {
      None
    } else if (spot(target).exists(_.owner == source.owner)) {
      None
    } else {
      Some(new Board(spots.updated(target.toIx, Some(source)).updated(from.toIx, None)))
    }
  }
}

object Board {
  def apply(): Board = new Board(
    Array(
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
      Some(Piece(P2, Pawn)),
      Some(Piece(P2, Pawn)),
      Some(Piece(P2, King)),
      Some(Piece(P2, Pawn)),
      Some(Piece(P2, Pawn))
    )
  )
}
