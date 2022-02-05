package wafna.games
package onitama

import cats.data.NonEmptyList
import wafna.games.minmax._
import wafna.games.Player.{P1, P2}

class OnitamaMinMax {

  implicit val onitamaMinMax: MinMax[Onitama] = new MinMax[Onitama] {

    override def currentPlayer(game: Onitama): Player = if (game.pass.isRight) P1 else P2

    override def evaluate(game: Onitama, player: Player): Int = game.gameOver match {
      case Some(Draw) => 0 // why not?
      case Some(Win(p)) => if (p == player) Int.MaxValue else Int.MinValue
      case None =>
        // todo worth favoring some early game optimizations?
        //   more interestingly, should evaluate be supplied by each player?
        0
    }

    override def moves(game: Onitama): Either[GameOver, NonEmptyList[Onitama]] = game.gameOver match {
      case None =>
        Right(game.moves())
      case Some(gameOver) =>
        Left(gameOver)
    }
  }
}

object OnitamaMinMax {

  def main(args: Array[String]): Unit = {
//    Arena.runGame(Onitama(),)
  }
}
