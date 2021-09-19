package wafna.mancala

import org.slf4j.LoggerFactory
import wafna.mancala.MancalaMinMax._
import wafna.minmax.MinMax
import wafna.minmax.MinMax.Eval

object Main extends App {

  private val log = LoggerFactory.getLogger(getClass)

  implicit val mmx: MinMax[Mancala] = MancalaMinMax

  private val g0: Mancala = Mancala(3, 3)
  log.info(g0.show())
  MinMax.search(g0, 5) match {
    case None =>
      log.info(s"NOTHING")
    case Some(Eval(game, eval)) =>
      log.info(s"SELECTED $eval\n${game.show()}")
  }
}
