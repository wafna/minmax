package wafna.minmax

import wafna.minmax.MinMax.{Eval, State}
import wafna.minmax.MinMax.State.{Drawn, Open, Won}
import wafna.util.Player
import wafna.util.Player.{P1, P2}

import scala.annotation.tailrec
import scala.util.Random
import scala.util.control.NoStackTrace

object Arena {

  class ArenaException(msg: String) extends Exception(msg) with NoStackTrace
  object ArenaException {
    def apply(msg: String): Nothing = throw new ArenaException(msg)
  }

  abstract class Bot[G](implicit minMax: MinMax[G]) {
    def show(): String
    def move(game: G): Option[Eval[G]]
  }
  class SearchBot[G](depth: Int)(implicit minMax: MinMax[G]) extends Bot[G] {
    override def show(): String = s"Search($depth)"
    override def move(game: G): Option[Eval[G]] = {
      MinMax.search(game, depth)
    }
  }
  class RandomBot[G]()(implicit minMax: MinMax[G]) extends Bot[G] {
    override def show(): String = "Random"
    override def move(game: G): Option[Eval[G]] = {
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

  case class Match(p1: String, p2: String, wins1: Int, wins2: Int, draws: Int)

  def runMatch[G](game: G, p1: Bot[G], p2: Bot[G], games: Int)(implicit minMax: MinMax[G]): Match = {
    require(0 < games)
    def keepScore(state: State): (Int, Int, Int) = {
      state match {
        case Open  => ArenaException(s"Open game.")
        case Drawn => (0, 0, 1)
        case Won(winner) =>
          winner match {
            case P1 => (1, 0, 0)
            case P2 => (0, 1, 0)
          }

      }
    }
    val score = Iterator.continually(game).take(games).foldLeft((0, 0, 0)) { (score, game) =>
      val score1 = keepScore(minMax.state(runGame(game, p1, p2)))
      val score2 = keepScore(minMax.state(runGame(game, p2, p1)))
      (score._1 + score1._1 + score2._2, score._2 + score1._2 + score2._1, score._3 + score1._3 + score2._3)
    }
    Match(p1.show(), p2.show(), score._1, score._2, score._3)
  }
}
