package wafna.mancala

import wafna.mancala.Mancala.{Score, move}
import wafna.util.Player
import wafna.util.Player._

import scala.annotation.tailrec

class Mancala private[mancala] (val currentPlayer: Player, val score: Score, pits1: Array[Int], pits2: Array[Int]) {

  import Mancala.copyPits

  def pass(): Mancala = {
    require(nextMoves.isEmpty)
    new Mancala(currentPlayer.opponent, score, pits1, pits2)
  }

  lazy val spots: (Array[Int], Array[Int]) = copyPits(pits1) -> (copyPits(pits2))

  lazy val nextMoves: Seq[Mancala] = {
    val pits = currentPlayer match {
      case P1 => pits1
      case P2 => pits2
    }
    pits.view.zipWithIndex.foldLeft(List.empty[Mancala]) { (moves, pit) =>
      val stonesInHand = pit._1
      if (0 == stonesInHand) {
        moves
      } else {
        val ps1 = copyPits(pits1)
        val ps2 = copyPits(pits2)
        val newScore = move(ps1, ps2, currentPlayer, score, pit._2)
        new Mancala(currentPlayer.opponent, newScore, ps1, ps2) :: moves
      }
    }
  }

  def show(): String = {
    val s = new StringBuilder
    s.append(s"score: (p1 = ${score.p1}, p2 = ${score.p2})\n")
    s.append(s"turn: ${currentPlayer}\n")
    val (pits1, pits2) = spots
    s.append(pits2.reverseIterator.map(n => n.formatted("%4d")).mkString)
    s.append("\n")
    s.append(pits1.view.map(n => n.formatted("%4d")).mkString)
    s.append("\n")
    s.toString()
  }
}

object Mancala {
  case class Score(p1: Int, p2: Int)
  def apply(pits: Int, stones: Int): Mancala = {
    require(1 < pits)
    require(0 < stones)
    new Mancala(P1, Score(0, 0), Array.fill(pits)(stones), Array.fill(pits)(stones))
  }
  private def copyPits(pits: Array[Int]): Array[Int] = {
    val copy = new Array[Int](pits.length)
    pits.copyToArray(copy)
    copy
  }
  private def move(pits1: Array[Int], pits2: Array[Int], currentPlayer: Player, score: Score, take: Int): Score = {

    def stuff(player: Player): (Array[Int], Int, Int) = player match {
      case P1 => (pits1, take + 1, pits1.length)
      case P2 => (pits2, take - 1, -1)
    }

    @tailrec
    def doMove(stonesInHand: Int, score: Score, side: Player, put: Int): Score = {
      val (pits, nextPit, scoringPit) = stuff(side)
      if (0 == stonesInHand) {
        score
      } else {
        if (put == scoringPit) {
          val (newScore, nextPit) = side match {
            case P1 => score.copy(p1 = score.p1 + 1) -> (pits.length - 1)
            case P2 => score.copy(p2 = score.p2 + 1) -> 0
          }
          doMove(stonesInHand - 1, newScore, side.opponent, nextPit)
        } else {
          pits(put) += 1
          doMove(stonesInHand, score, side, nextPit)
        }
      }
    }

    val (pits, nextPit, _) = stuff(currentPlayer)
    require(take < pits.length)

    val stonesInHand = pits(take)
    require(0 < stonesInHand)

    pits(take) = 0

    doMove(stonesInHand, score, currentPlayer, nextPit)
  }
}
