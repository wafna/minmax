package wafna.hexapawn

import wafna.hexapawn.Hexapawn.ShowPlayer
import wafna.minmax.MinMax
import wafna.util.Player

object HexapawnMinMax {
  type HexapawnMinMax = MinMax[Hexapawn, Player]
  implicit object HexapawnMinMaxTC extends HexapawnMinMax {

    override def currentPlayer(game: Hexapawn): Player =
      game.currentPlayer

    override def maximize(game: Hexapawn, player: Player): Boolean =
      player == game.currentPlayer

    override def evaluate(game: Hexapawn, player: Player): Int = {
      game.winner match {
        case None => 0
        case Some(endCon) =>
          endCon match {
            case None         => 0 // draw
            case Some(winner) => if (winner == player) 100 else -100
          }
      }
    }

    override def moves(game: Hexapawn): Seq[Hexapawn] =
      game.nextMoves

    override def pass(game: Hexapawn): Hexapawn =
      game.pass()

    override def show(game: Hexapawn): String =
      s"""player: ${game.currentPlayer.show()}
         |${Hexapawn.showGrid(game)}
         |""".stripMargin
  }
}
