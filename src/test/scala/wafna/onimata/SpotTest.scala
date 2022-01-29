package wafna.onimata

import wafna.TestBase

class SpotTest extends TestBase {

  "Spot" should {
    "be valid" in {
      (for {
        x <- 0 until 5
        y <- 0 until 5
      } yield (x, y)).foreach { case (x, y) =>
        Spot(x, y).valid shouldBe true
      }
    }
    "not be valid" in {
      Spot(-1, 0).valid shouldBe false
      Spot(0, -1).valid shouldBe false
      Spot(-1, -1).valid shouldBe false
      Spot(4, 5).valid shouldBe false
      Spot(5, 4).valid shouldBe false
      Spot(5, 5).valid shouldBe false
    }
    "toIx" in {
      Spot(0, 0).toIx shouldBe 0
      Spot(1, 0).toIx shouldBe 1
      Spot(0, 1).toIx shouldBe 5
      Spot(4, 1).toIx shouldBe 9
      Spot(4, 2).toIx shouldBe 14
      Spot(4, 4).toIx shouldBe 24
    }
    "fromIx" in {
      (for {
        x <- 0 until 5
        y <- 0 until 5
      } yield (x, y)).foreach { case (x, y) =>
        val spot = Spot(x, y)
        spot shouldBe Spot.fromIx(spot.toIx)
      }
    }
    "move" in {
      val origin = Spot(0, 0)
      (for {
        x <- -2 to 2
        y <- -2 to 2
      } yield (x, y)).foreach { case (x, y) =>
        val end = origin + Move(x, y)
        end.x shouldBe (origin.x + x)
        end.y shouldBe (origin.y + y)
      }
    }
  }
}
