package wafna.hexapawn

import wafna.hexapawn.Hexapawn.DrawCond
import wafna.minmax.MinMax
import wafna.minmax.MinMax.State
import wafna.util.Player

object HexapawnMinMax {

  implicit val hexapawnMinMax: MinMax[Hexapawn] = new MinMax[Hexapawn] {
    override def currentPlayer(game: Hexapawn): Player =
      game.currentPlayer
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
    override def state(game: Hexapawn): State =
      game.state match {
        case DrawCond.Passed => State.Open
        case DrawCond.Drawn  => State.Drawn
        case DrawCond.Open =>
          game.winner match {
            case None => State.Open
            case Some(w) =>
              w match {
                case None         => State.Drawn
                case Some(player) => State.Won(player)
              }
          }
      }
  }
}
