package wafna.games.onitama

import cats.data.NonEmptyList
import wafna.games.minmax.{Arena, GameOver, MinMax, Win}
import wafna.games.util
import wafna.games.util.Player.{P1, P2}

class OnitamaMinMax {

  implicit val onitamaMinMax: MinMax[Onitama] = new MinMax[Onitama] {
    override def currentPlayer(game: Onitama): util.Player = if (game.pass.isRight) P1 else P2

    override def evaluate(game: Onitama, player: util.Player): Int = ???

    override def moves(game: Onitama): Either[GameOver, NonEmptyList[Onitama]] = ???
  }
}

object OnitamaMinMax {

  def main(args: Array[String]): Unit = {
//    Arena.runGame(Onitama(),)
  }
}
