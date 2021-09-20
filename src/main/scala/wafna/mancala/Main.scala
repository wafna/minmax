package wafna.mancala

import org.slf4j.LoggerFactory
import wafna.mancala.MancalaMinMax._
import wafna.minmax.Arena.SearchBot
import wafna.minmax.{Arena, MinMax}

object Main extends App {

  private val log = LoggerFactory.getLogger(getClass)

  implicit val mmx: MinMax[Mancala] = MancalaMinMax

  private val g0: Mancala = Mancala(3, 3)
  log.info(Arena.runGame(g0, new SearchBot(3), new SearchBot(1)).show())
}
