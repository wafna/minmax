package wafna.games.onitama

import scala.collection.immutable.ArraySeq
import scala.util.Random

final case class Move(x: Int, y: Int)

case class Card(name: String, moves: ArraySeq[Move])

object Deck {
  
  val Monkey: Card = Card("Monkey", ArraySeq(Move(-1, -1), Move(1, -1), Move(-1, 1), Move(1, 1)))
  val Dragon: Card = Card("Dragon", ArraySeq(Move(-1, -1), Move(1, -1), Move(-2, 1), Move(2, 1)))
  val Tiger: Card = Card("Tiger", ArraySeq(Move(0, 2), Move(0, -1)))
  val Ox: Card = Card("Ox", ArraySeq(Move(1, 0), Move(0, -1), Move(0, 1)))
  val Horse: Card = Card("Horse", ArraySeq(Move(-1, 0), Move(0, -1), Move(0, 1)))
  val Goose: Card = Card("Goose", ArraySeq(Move(-1, 0), Move(-1, 1), Move(1, 0), Move(1, -1)))
  val Rooster: Card = Card("Rooster", ArraySeq(Move(-1, 0), Move(-1, -1), Move(1, 0), Move(1, 1)))
  val Cobra: Card = Card("Cobra", ArraySeq(Move(-1, 0), Move(1, 1), Move(1, -1)))
  val Eel: Card = Card("Eel", ArraySeq(Move(1, 0), Move(-1, 1), Move(-1, -1)))
  val Mantis: Card = Card("Mantis", ArraySeq(Move(-1, 1), Move(1, 1), Move(0, -1)))
  val Crane: Card = Card("Crane", ArraySeq(Move(-1, -1), Move(1, -1), Move(0, 1)))
  val Rabbit: Card = Card("Rabbit", ArraySeq(Move(-1, -1), Move(1, 1), Move(2, 0)))
  val Frog: Card = Card("Frog", ArraySeq(Move(1, -1), Move(-1, 1), Move(-2, 0)))
  val Boar: Card = Card("Boar", ArraySeq(Move(-1, 0), Move(1, 0), Move(0, 1)))
  val Crab: Card = Card("Crab", ArraySeq(Move(-2, 0), Move(0, 1), Move(2, 0)))
  val Elephant: Card = Card("Elephant", ArraySeq(Move(-1, 0), Move(-1, 1), Move(1, 0), Move(1, 1)))

  val cards: ArraySeq[Card] = ArraySeq(
    Monkey,
    Dragon,
    Tiger,
    Ox,
    Horse,
    Goose,
    Rooster,
    Cobra,
    Eel,
    Mantis,
    Crane,
    Rabbit,
    Frog,
    Boar,
    Crab,
    Elephant
  )
  def apply()(implicit random: Random = Random): (Hand, Hand, Card) = {
    val deck = random.shuffle(cards)
    (Hand(deck(0), deck(1)), Hand(deck(2), deck(3)), deck(4))
  }
}
