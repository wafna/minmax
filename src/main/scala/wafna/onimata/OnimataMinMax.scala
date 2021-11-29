package wafna.onimata

import wafna.minmax.MinMax
import wafna.util
import wafna.util.Player.{P1, P2}

class OnimataMinMax {
  implicit val hexapawnMinMax: MinMax[Onimata] = new MinMax[Onimata] {
    override def currentPlayer(game: Onimata): util.Player = if (game.pass.isRight) P1 else P2

    override def evaluate(game: Onimata, player: util.Player): Int = ???

    override def moves(game: Onimata): Seq[Onimata] = game.moves()

    override def pass(game: Onimata): Onimata = ???

    override def state(game: Onimata): MinMax.State = ???
  }
}
