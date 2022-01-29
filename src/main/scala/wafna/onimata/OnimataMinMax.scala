package wafna.onimata

import wafna.minmax.{GameOver, MinMax}
import wafna.util
import wafna.util.Player.{P1, P2}

class OnimataMinMax {
  implicit val hexapawnMinMax: MinMax[Onimata] = new MinMax[Onimata] {
    override def currentPlayer(game: Onimata): util.Player = if (game.pass.isRight) P1 else P2

    override def evaluate(game: Onimata, player: util.Player): Int = ???

    override def moves(game: Onimata): Either[GameOver, Seq[Onimata]] = ???
  }
}
