package jags

/**
 * Created by jose abelardo gutierrez on 11/30/15.
 */
object Game {

  private val board = new Board

  private def getPlayerInput: String = {
    val possibleMoves = board.possibleMoves
    print("Moves: ")
    for (i <- possibleMoves.indices) {
      print(i + 1 + ":" + possibleMoves(i) + " ")
    }
    while (true) {
      println("\nEnter your move number or 'q' to quit: ")
      for (input <- io.Source.stdin.getLines()) {
        if (input equalsIgnoreCase "q") {
          return input

        } else try {
          val num = input.toInt
          if (num >= 1 && num <= possibleMoves.length) {
            return input
          }
        } catch {
          case ex: NumberFormatException => // Ignore
        }
      }
    }
    throw new InvalidCodePathException
  }

  def play(): Unit = {

    while (true) {
      board.printBoard()
      val input = getPlayerInput
      if (input equalsIgnoreCase "q") {
        println("You quit!")
        return
      }
      val moveInput = input.toInt - 1
      val possibleMoves = board.possibleMoves
      val playerMove = possibleMoves(moveInput)
      board.setCell(playerMove, Board.CellState.Player)

      if (board.getWinner == Board.CellState.Player) {
        board.printBoard()
        println("You win this time!")
        return
      }

      if (board.countEmptyCells > 0) {
        val computerMove = board.getComputerMove
        board.setCell(computerMove, Board.CellState.Computer)
      }

      if (board.getWinner == Board.CellState.Computer) {
        board.printBoard()
        println("You loose the game!")
        return
      }

      if (board.isTie) {
        board.printBoard()
        println("The game ends in a tie...")
        return
      }
    }
  }
}
