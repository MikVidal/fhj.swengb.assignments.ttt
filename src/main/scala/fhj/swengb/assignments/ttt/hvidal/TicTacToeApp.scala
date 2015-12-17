package fhj.swengb.assignments.ttt.hvidal

/**
 * Implement here your TicTacToe JavaFX App.
 */

import java.net.URL
import java.util.ResourceBundle
import javafx.application.Application
import javafx.fxml.{FXML, Initializable, FXMLLoader}
import javafx.scene.{control, Parent, Scene}
import javafx.stage.Stage
import javafx.scene.layout.AnchorPane

import scala.util.control.NonFatal

/**
 * Implement here your TicTacToe JavaFX App.
 */

object TicTacToeApp {
  def main(args: Array[String]) {
    println("Launching TicTacToeApp")
    Application.launch(classOf[TicTacToeApp], args: _*)
  }
}

class TicTacToeApp extends javafx.application.Application {
  println("TicTacToeApp constructor")

  val Fxml = "/fhj/swengb/assignments/ttt/TicTacToeApp.fxml"
  //val Css = "/main/resources/fhj/swengb/assignments/ttt/TicTacToeApp.css"
  val loader = new FXMLLoader(getClass.getResource(Fxml))

  override def start(stage: Stage): Unit = try{
    stage.setTitle("TicTacToeApp")
    loader.load[Parent]() // side effect
    val scene = new Scene(loader.getRoot[Parent])
    stage.setScene(scene)
    stage.show()

  }catch {
    case NonFatal(e) => e.printStackTrace()
  }
}

class TicTacToeAppController extends TicTacToeApp {

  @FXML var window: AnchorPane = _
  //@FXML var newGame: control.Button = _
  //@FXML var gameMod: control.Button = _ //switch to multigame mode to v.s Computer mode
  //@FXML var history: control.Button = _
  //@FXML var hint: control.Button = _

  @FXML var panel: control.Label = _

  @FXML var topLeft: control.Button= _
  @FXML var topCenter: control.Button= _
  @FXML var topRight: control.Button= _
  @FXML var middleLeft: control.Button= _
  @FXML var middleCenter: control.Button= _
  @FXML var middleRight: control.Button= _
  @FXML var bottomLeft: control.Button= _
  @FXML var bottomCenter: control.Button= _
  @FXML var bottomRight: control.Button= _

  var board = new Array[Char](9)

  var playerA: Char = '0'
  var playerB: Char = 'X'
  var whoIsPlaying: Char = ' '

  // var ll=List(topLeft,topCenter, topRight, middleLeft, middleCenter, middleRight, bottomLeft, bottomCenter, bottomRight)
  //var positionMarked: Int = null


  def newGame(): Unit ={
    panel.setText("New Game - Multiplayer")
    topLeft.setText(" ")
    topCenter.setText(" ")
    topRight.setText(" ")
    middleLeft.setText(" ")
    middleCenter.setText(" ")
    middleRight.setText(" ")
    bottomLeft.setText(" ")
    bottomCenter.setText(" ")
    bottomRight.setText(" ")
    //var board = new Array[Char](9)
    //window.getScene().getWindow().hide() ; start(new Stage);
    whoIsPlaying = playerA
  }

  def changePlayer(): Unit={
    if (whoIsPlaying == playerA){whoIsPlaying = playerB}
    else {whoIsPlaying = playerA}
    //println(whoIsPlaying)
  }

  def writeOnBoard(whoIsPlaying: Char, postionMarked: Int): Unit = {
    if (postionMarked == 0) {
      topLeft.setText(whoIsPlaying.toString)
    }
    else if (postionMarked == 1) {
      topCenter.setText(whoIsPlaying.toString)
    }
    else if (postionMarked == 2) {
      topRight.setText(whoIsPlaying.toString)
    }
    else if (postionMarked == 3) {
      middleLeft.setText(whoIsPlaying.toString)
    }
    else if (postionMarked == 4) {
      middleCenter.setText(whoIsPlaying.toString)
    }
    else if (postionMarked == 5) {
      middleRight.setText(whoIsPlaying.toString)
    }
    else if (postionMarked == 6) {
      bottomLeft.setText(whoIsPlaying.toString)
    }
    else if (postionMarked == 7) {
      bottomCenter.setText(whoIsPlaying.toString)
    }
    else {
      bottomRight.setText(whoIsPlaying.toString)
    }
  }

  //https://github.com/trptcolin/tictactoe-scala/blob/master/src/trptcolin/tictactoescala/basegame/Board.scala
  def checkWinner(board: Array[Char], whoIsPlaying: Char): Boolean={
    val winSets =
      List(
        List(0, 1, 2),
        List(3, 4, 5),
        List(6, 7, 8),
        List(0, 3, 6),
        List(1, 4, 7),
        List(2, 5, 8),
        List(0, 4, 8),
        List(2, 4, 6))
    winSets.exists(winSet => winSet.forall(board(_)==whoIsPlaying))

  }
  def endGame(whoIsPlaying: Char): Unit={

    //ll.foreach((i: Char) => i.setText(whoIsPlaying.toString)

    topLeft.setText(whoIsPlaying.toString)
    topCenter.setText(whoIsPlaying.toString)
    topRight.setText(whoIsPlaying.toString)
    middleLeft.setText(whoIsPlaying.toString)
    middleCenter.setText(whoIsPlaying.toString)
    middleRight.setText(whoIsPlaying.toString)
    bottomLeft.setText(whoIsPlaying.toString)
    bottomCenter.setText(whoIsPlaying.toString)
    bottomRight.setText(whoIsPlaying.toString)
    println("GameOver\n")

  }

  def printMovement(board: Array[Char]): Unit={
    val divider = "|---|---|---|\n"

    print(divider+"|-"+board(0)+"-|-"+board(1)+"-|-"+board(2)+"-|\n"+divider+
      "|-"+board(3)+"-|-"+board(4)+"-|-"+board(5)+"-|\n"+divider+
      "|-"+board(6)+"-|-"+board(7)+"-|-"+board(8)+"-|\n"+divider+"\n")
  }

  def main(whoIsPlaying: Char, positionMarked: Int, board: Array[Char]): Unit ={
    board(positionMarked)= whoIsPlaying
    writeOnBoard(whoIsPlaying,positionMarked)
    changePlayer()
    printMovement(board)
    if(checkWinner(board,whoIsPlaying)){
      panel.setText("Player "+ whoIsPlaying+" won the Game")
      endGame(whoIsPlaying)

    }
  }

  def topLeftMarked(): Unit = {
    main(whoIsPlaying,0,board)
  }
  def topCenterMarked(): Unit = {
    main(whoIsPlaying,1,board)
  }
  def topRightMarked(): Unit = {
    main(whoIsPlaying,2,board)
  }
  def middleLeftMarked(): Unit =  {
    main(whoIsPlaying,3,board)
  }
  def middleCenterMarked(): Unit = {
    main(whoIsPlaying,4,board)
  }
  def middleRightMarked(): Unit = {
    main(whoIsPlaying,5,board)
  }
  def bottomLeftMarked(): Unit = {
    main(whoIsPlaying,6,board)
  }
  def bottomCenterMarked(): Unit = {
    main(whoIsPlaying,7,board)
  }
  def bottomRightMarked(): Unit = {
    main(whoIsPlaying,8,board)
  }

  def gameMode(): Unit = ???

}