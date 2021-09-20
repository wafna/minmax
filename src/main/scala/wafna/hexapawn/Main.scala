package wafna.hexapawn

import org.slf4j.LoggerFactory
import wafna.hexapawn.HexapawnMinMax._
import wafna.minmax.Arena
import wafna.minmax.Arena.{RandomBot, SearchBot}

object Main extends App {

  private val log = LoggerFactory.getLogger(getClass)

  private val g0: Hexapawn = Hexapawn(7, 3)
  log.info(Arena.runGame(g0, new SearchBot(6), new RandomBot()).show())
}
