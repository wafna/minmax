package wafna.minmax

import wafna.minmax.MinMax.Eval

import scala.annotation.tailrec
import scala.util.Random

object Arena {

  abstract class Bot[G](implicit minMax: MinMax[G]) {
    def move(game: G): Option[Eval[G]]
  }
  class SearchBot[G](depth: Int)(implicit minMax: MinMax[G]) extends Bot[G] {
    def move(game: G): Option[Eval[G]] = {
      MinMax.search(game, depth)
    }
  }
  class RandomBot[G]()(implicit minMax: MinMax[G]) extends Bot[G] {
    def move(game: G): Option[Eval[G]] = {
      val moves = minMax.moves(game)
      if (moves.isEmpty) {
        None
      } else {
        Some(Eval(moves(new Random().nextInt(moves.length)), 0))
      }
    }
  }

  def runGame[G](game: G, p1: Bot[G], p2: Bot[G])(implicit minMax: MinMax[G]): G = {
    @tailrec
    def runPlayer(game: G, bots: LazyList[Bot[G]])(implicit minMax: MinMax[G]): G = {
      bots.head.move(game) match {
        case None =>
          game
        case Some(move) =>
          runPlayer(move.game, bots.tail)
      }
    }
    runPlayer(game, LazyList.continually(Seq(p1, p2)).flatten)
  }
}
