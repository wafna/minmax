package wafna.games
package onitama

import cats.data.NonEmptyList

import scala.collection.immutable.ArraySeq
import scala.util.Random

final case class Move(x: Int, y: Int)

case class Card(name: String, moves: NonEmptyList[Move])

object Deck {
  
  val Monkey: Card = Card("Monkey", nonEmptyList(Move(-1, -1), Move(1, -1), Move(-1, 1), Move(1, 1)))
  val Dragon: Card = Card("Dragon", nonEmptyList(Move(-1, -1), Move(1, -1), Move(-2, 1), Move(2, 1)))
  val Tiger: Card = Card("Tiger", nonEmptyList(Move(0, 2), Move(0, -1)))
  val Ox: Card = Card("Ox", nonEmptyList(Move(1, 0), Move(0, -1), Move(0, 1)))
  val Horse: Card = Card("Horse", nonEmptyList(Move(-1, 0), Move(0, -1), Move(0, 1)))
  val Goose: Card = Card("Goose", nonEmptyList(Move(-1, 0), Move(-1, 1), Move(1, 0), Move(1, -1)))
  val Rooster: Card = Card("Rooster", nonEmptyList(Move(-1, 0), Move(-1, -1), Move(1, 0), Move(1, 1)))
  val Cobra: Card = Card("Cobra", nonEmptyList(Move(-1, 0), Move(1, 1), Move(1, -1)))
  val Eel: Card = Card("Eel", nonEmptyList(Move(1, 0), Move(-1, 1), Move(-1, -1)))
  val Mantis: Card = Card("Mantis", nonEmptyList(Move(-1, 1), Move(1, 1), Move(0, -1)))
  val Crane: Card = Card("Crane", nonEmptyList(Move(-1, -1), Move(1, -1), Move(0, 1)))
  val Rabbit: Card = Card("Rabbit", nonEmptyList(Move(-1, -1), Move(1, 1), Move(2, 0)))
  val Frog: Card = Card("Frog", nonEmptyList(Move(1, -1), Move(-1, 1), Move(-2, 0)))
  val Boar: Card = Card("Boar", nonEmptyList(Move(-1, 0), Move(1, 0), Move(0, 1)))
  val Crab: Card = Card("Crab", nonEmptyList(Move(-2, 0), Move(0, 1), Move(2, 0)))
  val Elephant: Card = Card("Elephant", nonEmptyList(Move(-1, 0), Move(-1, 1), Move(1, 0), Move(1, 1)))

  val cards: NonEmptyList[Card] = nonEmptyList(
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
    val deck = random.shuffle(cards.toList)
    (Hand(deck(0), deck(1)), Hand(deck(2), deck(3)), deck(4))
  }
}
