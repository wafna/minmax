package wafna.minmax

import wafna.minmax.MinMax.Eval
import wafna.util.Player.{P1, P2}

import scala.annotation.tailrec
import scala.util.Random

object Arena {

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
    val score = Iterator.continually(game).take(games).foldLeft((0, 0, 0)) { (score, game) =>
      val score1 = minMax.winner(runGame(game, p1, p2)) match {
        case None => (score._1, score._2, score._3 + 1)
        case Some(winner) =>
          winner match {
            case P1 => (score._1 + 1, score._2, score._3)
            case P2 => (score._1, score._2 + 1, score._3)
          }

      }
      minMax.winner(runGame(game, p2, p1)) match {
        case None => (score1._1, score1._2, score1._3 + 1)
        case Some(winner) =>
          winner match {
            case P1 => (score1._1, score1._2 + 1, score1._3)
            case P2 => (score1._1 + 1, score1._2, score1._3)
          }

      }
    }
    Match(p1.show(), p2.show(), score._1, score._2, score._3)
  }
}
