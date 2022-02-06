package wafna
package games
package onitama

import wafna.games.onitama.Deck.*

import scala.collection.immutable.ArraySeq

class Scenario1Test extends TestBase {
  "Scenario 1" should {
    /*
      This is the scene:

      P1: Tiger, Eel
      P2: Rabbit, Crab
      Pass: P1 Rooster

      oo-oo
      .....
      ..O..
      ...Xx
      xx-x.

    Here are the cards:

      Tiger    Crab     Rabbit   Rooster  Eel
        X
                 X         X        X      X
        O      X O X      O X     XOX       OX
        X                X        X        X

    X (P1) can use Eel to kill O's (P2) king and win.

     */
    "P1 to win in one move" in {
      val g = new Onitama(
        Hand(Tiger, Eel),
        Hand(Rabbit, Crab),
        Right(Rooster),
        new Board(
          ArraySeq(
            Some(Piece(P2, Pawn)),
            Some(Piece(P2, Pawn)),
            None,
            Some(Piece(P2, Pawn)),
            Some(Piece(P2, Pawn)),
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some(Piece(P2, King)),
            None,
            None,
            None,
            None,
            None,
            Some(Piece(P1, King)),
            Some(Piece(P1, Pawn)),
            Some(Piece(P1, Pawn)),
            Some(Piece(P1, Pawn)),
            None,
            Some(Piece(P1, Pawn)),
            None
          )
        )
      )
      assertResult(P1)(g.currentPlayer)
    }
  }
}
