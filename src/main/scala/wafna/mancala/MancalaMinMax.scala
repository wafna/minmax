package wafna.mancala

import wafna.minmax.MinMax
import wafna.util.Player
import wafna.util.Player.{P1, P2}

object MancalaMinMax {

  type MancalaMinMax = MinMax[Mancala]

  implicit object MancalaMinMax extends MinMax[Mancala] {

    override def currentPlayer(game: Mancala): Player =
      game.currentPlayer

    override def evaluate(game: Mancala, player: Player): Int = {
      val score = game.score
      player match {
        case P1 => score.p1 - score.p2
        case P2 => score.p2 - score.p1
      }
    }

    override def moves(game: Mancala): Seq[Mancala] = game.nextMoves

    override def pass(game: Mancala): Mancala = game.pass

    override def show(game: Mancala): String = game.show
  }
}
