package wafna.hexapawn

import org.slf4j.LoggerFactory
import wafna.hexapawn.Hexapawn.ShowPlayer
import wafna.hexapawn.HexapawnMinMax.HexapawnMinMax
import wafna.minmax.MinMax
import wafna.minmax.MinMax.Eval
import wafna.util.Player

import scala.annotation.tailrec

object Arena {
  private val log = LoggerFactory.getLogger(getClass)
  class Bot(depth: Int) {
    def move(game: Hexapawn)(implicit minMax: HexapawnMinMax): Option[Eval[Hexapawn]] = {
      MinMax.search(game, depth)
    }
  }

  def runMatch(game: Hexapawn, p1: Bot, p2: Bot)(implicit minMax: HexapawnMinMax): Option[Option[Player]] = {
    @tailrec
    def runPlayer(game: Hexapawn, bots: LazyList[Bot])(implicit minMax: HexapawnMinMax): Option[Option[Player]] = {
      bots.head.move(game) match {
        case None =>
          log.info(s"WINNER? ${game.winner}")
          game.winner
        case Some(move) =>
          log.info(s"PLAYER ${move.game.currentPlayer.show()} MOVES\n${move.game.show()}")
          runPlayer(move.game, bots.tail)
      }
    }
    runPlayer(game, LazyList.continually(Seq(p1, p2)).flatten)
  }

}
