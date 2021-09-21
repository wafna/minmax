package wafna.hexapawn

import org.slf4j.LoggerFactory
import wafna.hexapawn.HexapawnMinMax._
import wafna.minmax.Arena
import wafna.minmax.Arena.{RandomBot, SearchBot}

object Main extends App {
  private val log = LoggerFactory.getLogger(getClass)
  private val g0: Hexapawn = Hexapawn(3, 3)
  log.info(Arena.runMatch(g0, new SearchBot(6), new RandomBot(), 100).toString)
}
