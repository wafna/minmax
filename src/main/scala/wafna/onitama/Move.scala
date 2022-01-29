package wafna.onitama

import scala.collection.immutable.ArraySeq
import scala.util.Random

final case class Move(x: Int, y: Int)

case class Card(name: String, moves: ArraySeq[Move])

object Deck {
  val cards: ArraySeq[Card] = ArraySeq(
    Card("Monkey", ArraySeq(Move(-1, -1), Move(1, -1), Move(-1, 1), Move(1, 1))),
    Card("Dragon", ArraySeq(Move(-1, -1), Move(1, -1), Move(-2, 1), Move(2, 1))),
    Card("Tiger", ArraySeq(Move(0, 2), Move(0, -1))),
    Card("Ox", ArraySeq(Move(1, 0), Move(0, -1), Move(0, 1))),
    Card("Horse", ArraySeq(Move(-1, 0), Move(0, -1), Move(0, 1))),
    Card("Goose", ArraySeq(Move(-1, 0), Move(-1, 1), Move(1, 0), Move(1, -1))),
    Card("Rooster", ArraySeq(Move(-1, 0), Move(-1, -1), Move(1, 0), Move(1, 1))),
    Card("Cobra", ArraySeq(Move(-1, 0), Move(1, 1), Move(1, -1))),
    Card("Eel", ArraySeq(Move(1, 0), Move(-1, 1), Move(-1, -1))),
    Card("Mantis", ArraySeq(Move(-1, 1), Move(1, 1), Move(0, -1))),
    Card("Crane", ArraySeq(Move(-1, -1), Move(1, -1), Move(0, 1))),
    Card("Rabbit", ArraySeq(Move(-1, -1), Move(1, 1), Move(2, 0))),
    Card("Frog", ArraySeq(Move(1, -1), Move(-1, 1), Move(-2, 0))),
    Card("Boar", ArraySeq(Move(-1, 0), Move(1, 0), Move(0, 1))),
    Card("Crab", ArraySeq(Move(-2, 0), Move(0, 1), Move(2, 0))),
    Card("Elephant", ArraySeq(Move(-1, 0), Move(-1, 1), Move(1, 0), Move(1, 1)))
  )
  def apply()(implicit random: Random = Random): (Hand, Hand, Card) = {
    val deck = random.shuffle(cards)
    (Hand(deck(0), deck(1)), Hand(deck(2), deck(3)), deck(4))
  }
}
