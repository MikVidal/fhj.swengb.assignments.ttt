package fhj.swengb.assignments.ttt.hvidal

import scala.collection.Set

/**
  * models the different moves the game allows
  *
  * each move is made by either player a or player b.
  */

sealed trait TMove {
  def idx: Int
}

case object TopLeft extends TMove {
  override def idx: Int = 0
}

case object TopCenter extends TMove {
  override def idx: Int = 1
}

case object TopRight extends TMove {
  override def idx: Int = 2
}

case object MiddleLeft extends TMove {
  override def idx: Int = 3
}

case object MiddleCenter extends TMove {
  override def idx: Int = 4
}

case object MiddleRight extends TMove {
  override def idx: Int = 5
}

case object BottomLeft extends TMove {
  override def idx: Int = 6
}

case object BottomCenter extends TMove {
  override def idx: Int = 7
}

case object BottomRight extends TMove {
  override def idx: Int = 8
}


/**
  * for a tic tac toe game, there are two players, player A and player B
  */
sealed trait Player

case object PlayerA extends Player

case object PlayerB extends Player

//case object empty extends Player



object TicTacToe {

  //var board = List(TopLeft, TopCenter, TopRight, MiddleLeft, MiddleCenter, MiddleRight, BottomLeft, BottomCenter, BottomRight)
  //val PlayerA = PlayerA("0")
  //val PlayerB = PlayerB("X")

  /**
   * creates an empty tic tac toe game
   * @return
   */
  def apply(): TicTacToe = { new TicTacToe(Map.empty[TMove,Player])
    /**TicTacToe(Map((TopLeft,empty),(TopCenter, empty),(TopRight, empty),
    (MiddleLeft, empty),(MiddleCenter, empty),(MiddleRight, empty),
    (BottomLeft, empty),(BottomCenter, empty),(BottomRight, empty)))
      **/
  }

  /**
   * For a given tic tac toe game, this function applies all moves to the game.
   * The first element of the sequence is also the first move.
   *
   * @param t
   * @param moves
   * @return
   */
  def play(t: TicTacToe, moves: Seq[TMove]): TicTacToe = {
    def changePlayer(player: Player): Player = {
      player match{
        case PlayerA => PlayerB
        case PlayerB => PlayerA
      }
    }
    val nextPlayer = changePlayer(t.nextPlayer)
    val movements = moves.foldLeft(Map.empty[TMove, Player])((map,value) => map + (value -> changePlayer(nextPlayer)))
    val ttt = TicTacToe(movements,changePlayer(movements.last._2))
    ttt
  }

  /**
   * creates all possible games.
   * @return
   */
  def mkGames(): Map[Seq[TMove], TicTacToe] = ???

}

/**
 * Models the well known tic tac toe game.
 *
 * The map holds the information which player controls which field.
 *
 * The nextplayer parameter defines which player makes the next move.
 */
case class TicTacToe(moveHistory: Map[TMove, Player],
                     nextPlayer: Player = PlayerA) {

  /**
   * outputs a representation of the tic tac toe like this:
   *
   * |---|---|---|
   * | x | o | x |
   * |---|---|---|
   * | o | x | x |
   * |---|---|---|
   * | x | o | o |
   * |---|---|---|
   *
   * ()
   * @return
   */
  def asString(): String = {
    def Xor0(m: TMove): String = moveHistory match {
      case not if !moveHistory.contains(m) => " "
      case x if x.apply(m).equals(PlayerA) => "X"
      case o if o.apply(m).equals(PlayerB) => "O"
    }
        val divider = "|---|---|---|\n"
        val boardString = divider + "|-" + Xor0(TopLeft) + "-|-" + Xor0(TopCenter) + "-|-" + Xor0(TopRight) + "-|\n" + divider +
          "|-" + Xor0(MiddleLeft) + "-|-" + Xor0(MiddleCenter) + "-|-" + Xor0(MiddleRight) + "-|\n" + divider +
          "|-" + Xor0(BottomLeft) + "-|-" + Xor0(BottomCenter) + "-|-" + Xor0(BottomRight) + "-|\n" + divider + "\n"
        boardString
  }
  /**
   * the moves which are still to be played on this tic tac toe.
   */
  val remainingMoves: Set[TMove] = {
    val moves: Set[TMove] = Set(TopLeft, TopCenter, TopRight, MiddleLeft, MiddleCenter, MiddleRight, BottomLeft, BottomCenter, BottomRight)
    moves.diff(moveHistory.keySet)
  }

  /**
   * Either there is no winner, or PlayerA or PlayerB won the game.
   * The set of moves contains all moves which contributed to the result.
   */
  def winner: Option[(Player, Set[TMove])] = {
    val winSets: List[Set[TMove]] = List(Set(TopLeft, TopCenter, TopRight), 
      Set(MiddleLeft, MiddleCenter, MiddleRight),
      Set(BottomLeft, BottomCenter, BottomRight),
      Set(TopLeft, MiddleCenter, BottomRight),
      Set(TopRight, MiddleCenter, BottomLeft),
      Set(TopCenter, MiddleCenter, BottomCenter),
      Set(TopLeft, MiddleLeft, BottomLeft),
      Set(TopRight, MiddleRight, BottomRight))
    
    def checkWinner(player: Player): Boolean = {

      if(winSets.head.subsetOf(moveHistory.filter(_._2 == player).keySet))
        true
      else if(winSets(1).subsetOf(moveHistory.filter(_._2 == player).keySet))
        true
      else if(winSets(2).subsetOf(moveHistory.filter(_._2 == player).keySet))
        true
      else if(winSets(3).subsetOf(moveHistory.filter(_._2 == player).keySet))
        true
      else if(winSets(4).subsetOf(moveHistory.filter(_._2 == player).keySet))
        true
      else if(winSets(5).subsetOf(moveHistory.filter(_._2 == player).keySet))
        true
      else if(winSets(6).subsetOf(moveHistory.filter(_._2 == player).keySet))
        true
      else if(winSets(7).subsetOf(moveHistory.filter(_._2 == player).keySet))
        true
      else
        false
    }
    if(checkWinner(PlayerA))
      Some(PlayerA,moveHistory.filter(_._2 == PlayerA).keySet)
    else if(checkWinner(PlayerB))
      Some(PlayerB,moveHistory.filter(_._2 == PlayerB).keySet)
    else None

  }

    /**
     * is true if the game is over.
     *
     * The game is over if either of a player wins or there is a draw.
     */
    val gameOver: Boolean = {
      if (remainingMoves.isEmpty) true
      else if(winner.isDefined) true
      else false
    }


    /**
     * given a tic tac toe game, this function returns all
     * games which can be derived by making the next turn. that means one of the
     * possible turns is taken and added to the set.
     */
    lazy val nextGames: Set[TicTacToe] = {
      nextGames.+(TicTacToe(moveHistory: Map[TMove, Player]))
    }


  /**
     * returns a copy of the current game, but with the move applied to the tic tac toe game.
     *
     * @param move to be played
     * @param player the player
     * @return
     */

    def turn(move: TMove, player: Player): TicTacToe = {
      if (player.equals(PlayerA)) {
        TicTacToe(moveHistory + (move -> player), PlayerB)
      }
      else {
        TicTacToe(moveHistory + (move -> player), PlayerA)
      }

    }

  }


