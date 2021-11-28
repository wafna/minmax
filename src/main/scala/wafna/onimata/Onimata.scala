package wafna.onimata

import scala.collection.immutable.ArraySeq

case class Hand(c1: Card, c2: Card)

sealed trait Player
case object P1 extends Player
case object P2 extends Player

sealed trait Kind
case object Pawn extends Kind
case object King extends Kind

case class Piece(owner: Player, kind: Kind)

final case class Spot(x: Int, y: Int) {
  def valid: Boolean = 0 <= x && x < 5 && 0 <= y && y < 5
  def +(spot: Spot): Spot = Spot(x + spot.x, y + spot.y)
  def toIx: Int = x + (5 * y)
}

final case class Move(x: Int, y: Int)

sealed abstract class Card(val name: String, val moves: ArraySeq[Move])
case object Monkey extends Card("Monkey", ArraySeq(Move(-1, -1), Move(1, -1), Move(-1, 1), Move(1, 1)))
case object Dragon extends Card("Dragon", ArraySeq(Move(-1, -1), Move(1, -1), Move(-2, 1), Move(2, 1)))
case object Tiger extends Card("Tiger", ArraySeq(Move(0, 2), Move(0, -1)))
case object Ox extends Card("Ox", ArraySeq(Move(1, 0), Move(0, -1), Move(0, 1)))
case object Horse extends Card("Horse", ArraySeq(Move(-1, 0), Move(0, -1), Move(0, 1)))
case object Goose extends Card("Goose", ArraySeq(Move(-1, 0), Move(-1, 1), Move(1, 0), Move(1, -1)))
case object Rooster extends Card("Rooster", ArraySeq(Move(-1, 0), Move(-1, -11), Move(1, 0), Move(1, 1)))
case object Cobra extends Card("Cobra", ArraySeq(Move(-1, 0), Move(1, 1), Move(1, -1)))
case object Eel extends Card("Eel", ArraySeq(Move(1, 0), Move(-1, 1), Move(-1, -1)))
case object Mantis extends Card("Mantis", ArraySeq(Move(-1, 1), Move(1, 1), Move(0, -1)))
case object Crane extends Card("Crane", ArraySeq(Move(-1, -1), Move(1, -1), Move(0, 1)))
case object Rabbit extends Card("Rabbit", ArraySeq(Move(-1, -1), Move(1, 1), Move(2, 0)))
case object Frog extends Card("Frog", ArraySeq(Move(1, -1), Move(-1, 1), Move(-2, 0)))
case object Boar extends Card("Boar", ArraySeq(Move(-1, 0), Move(1, 0), Move(0, 1)))
case object Crab extends Card("Crab", ArraySeq(Move(-2, 0), Move(1, 0), Move(0, 2)))
case object Elephant extends Card("Elephant", ArraySeq(Move(-1, 0), Move(-1, 1), Move(1, 0), Move(1, 1)))

object Card {}

// The direction of the pass indicates the player with turn in hand.
class Onimata private (p1: Hand, p2: Hand, pass: Either[Card, Card], board: Board) {
  def moves(): List[Onimata] = {
    val Hand(c1, c2) = pass match {
      case Right(_) => p1
      case Left(_)  => p2
    }
  }
}

object Onimata {
  def apply(p1: Hand, p2: Hand, pass: Card): Onimata = new Onimata(p1, p2, Right(pass), Board())
}
