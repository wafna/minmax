package wafna.mancala

import wafna.minmax.MinMax
import wafna.minmax.MinMax.State
import wafna.util.Player
import wafna.util.Player.{P1, P2}

object MancalaMinMax {

  implicit val mancalaMinMax = new MinMax[Mancala] {
    override def currentPlayer(game: Mancala): Player =
      game.currentPlayer
    override def evaluate(game: Mancala, player: Player): Int = {
      val score = game.score
      player match {
        case P1 => score.p1 - score.p2
        case P2 => score.p2 - score.p1
      }
    }
    override def moves(game: Mancala): Seq[Mancala] =
      game.nextMoves
    override def pass(game: Mancala): Mancala =
      game.pass()

    override def state(game: Mancala): State =
      if (game.score.p1 > game.score.p2) {
        State.Won(P1)
      } else if (game.score.p1 < game.score.p2) {
        State.Won(P2)
      } else {
        State.Drawn
      }
  }
}
