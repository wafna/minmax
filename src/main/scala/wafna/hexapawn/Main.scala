package wafna.hexapawn

import org.slf4j.LoggerFactory
import wafna.hexapawn.HexapawnMinMax._
import wafna.minmax.MinMax
import wafna.minmax.MinMax.Eval

object Main extends App {

  private val log = LoggerFactory.getLogger(getClass)

  private val g0: Hexapawn = Hexapawn(3, 3)
  // Arena.runMatch(g0, new Bot(6), new Bot(2))
  MinMax.search(g0, 5) match {
    case None =>
      log.info(s"NOTHING")
    case Some(Eval(game, eval)) =>
      log.info(s"SELECTED $eval\n${game.show()}")
  }
}
