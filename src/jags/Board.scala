package jags

import jags.Board.CellState.{Computer, Empty, Player}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * Created by jose abelardo gutierrez on 11/30/15.
 */
object Board {

  private val random = new Random

  trait CellState

  object CellState {

    case object Player extends CellState

    case object Computer extends CellState

    case object Empty extends CellState

  }

  private val DIM = 3

}

class Board {

  private val cells = Array.fill[Board.CellState](Board.DIM, Board.DIM)(Board.CellState.Empty)

  def setCell(cell: (Int, Int), state: Board.CellState) = {
    cells(cell._1)(cell._2) = state
  }

  def getWinner: Board.CellState = {
    for (row <- 0 until Board.DIM) {
      if (cells(row)(0) != Board.CellState.Empty
          && cells(row)(0) == cells(row)(1)
          && cells(row)(0) == cells(row)(2)) {
        return cells(row)(0)
      }
    }
    for (column <- 0 until Board.DIM) {
      if (cells(0)(column) != Board.CellState.Empty
          && cells(0)(column) == cells(1)(column)
          && cells(0)(column) == cells(2)(column)) {
        return cells(0)(column)
      }
    }
    if (cells(0)(0) != Board.CellState.Empty
        && cells(0)(0) == cells(1)(1)
        && cells(0)(0) == cells(2)(2)) {
      return cells(0)(0)
    }
    if (cells(0)(2) != Board.CellState.Empty
        && cells(0)(2) == cells(1)(1)
        && cells(0)(2) == cells(2)(0)) {
      return cells(0)(2)
    }
    Board.CellState.Empty
  }

  def isTie = getWinner == Board.CellState.Empty && countEmptyCells == 0

  def countEmptyCells = {
    val sum = (0 /: cells) {(sum, row) => row.count(_ == Board.CellState.Empty)}
    sum
  }

  def possibleMoves = {
    val builder = new ArrayBuffer[(Int, Int)]()
    for (row <- 0 until Board.DIM) {
      for (column <- 0 until Board.DIM) {
        if (cells(row)(column) == Board.CellState.Empty) {
          builder += Tuple2(row, column)
        }
      }
    }
    builder.toArray
  }

  def printBoard() {
    println("  0 1 2")
    for (rowIdx <- cells.indices) {
      print(rowIdx)
      val row = cells(rowIdx)
      for (columnIdx <- row.indices) {
        row(columnIdx) match {
          case Player => print(" X")
          case Computer => print(" O")
          case Empty => print(" .")
        }
      }
      println()
    }
  }

  def getLineMoveRow(state: Board.CellState, row: Int, columnA: Int, columnB: Int) = {
    val columnC = 3 - columnA - columnB
    if (cells(row)(columnA) == state &&
        cells(row)(columnA) == cells(row)(columnB) &&
        cells(row)(columnC) == Board.CellState.Empty) {
      (row, columnC)
    } else {
      null
    }
  }

  def getLineMoveColumn(state: Board.CellState, column: Int, rowA: Int, rowB: Int) = {
    val rowC = 3 - rowA - rowB
    if (cells(rowA)(column) == state &&
        cells(rowA)(column) == cells(rowB)(column) &&
        cells(rowC)(column) == Board.CellState.Empty) {
      (rowC, column)
    } else {
      null
    }
  }

  def getLineMoveDiagonal(state: Board.CellState, cellA: Int, cellB: Int) = {
    val cellC = 3 - cellA - cellB
    if (cells(cellA)(cellA) == state &&
        cells(cellA)(cellA) == cells(cellB)(cellB) &&
        cells(cellC)(cellC) == Board.CellState.Empty) {
      (cellC, cellC)
    } else {
      null
    }
  }

  def getLineMoveInvDiagonal(state: Board.CellState, columnA: Int, columnB: Int) = {
    val columnC = 3 - columnA - columnB
    val rowA = 2 - columnA
    val rowB = 2 - columnB
    val rowC = 2 - columnC
    if (cells(rowA)(columnA) == state &&
        cells(rowA)(columnA) == cells(rowB)(columnB) &&
        cells(rowC)(columnC) == Board.CellState.Empty) {
      (rowC, columnC)
    } else {
      null
    }
  }

  def getLineMove(state: Board.CellState): (Int, Int) = {
    var result: (Int, Int) = null

    for (row <- 0 until Board.DIM) {
      result = getLineMoveRow(state, row, 0, 1)
      if (result != null) return result

      result = getLineMoveRow(state, row, 1, 2)
      if (result != null) return result

      result = getLineMoveRow(state, row, 0, 2)
      if (result != null) return result
    }

    for (column <- 0 until Board.DIM) {
      result = getLineMoveColumn(state, column, 0, 1)
      if (result != null) return result

      result = getLineMoveColumn(state, column, 1, 2)
      if (result != null) return result

      result = getLineMoveColumn(state, column, 0, 2)
      if (result != null) return result
    }

    result = getLineMoveDiagonal(state, 0, 1)
    if (result != null) return result

    result = getLineMoveDiagonal(state, 1, 2)
    if (result != null) return result

    result = getLineMoveDiagonal(state, 0, 2)
    if (result != null) return result

    result = getLineMoveInvDiagonal(state, 0, 1)
    if (result != null) return result

    result = getLineMoveInvDiagonal(state, 1, 2)
    if (result != null) return result

    getLineMoveInvDiagonal(state, 0, 2)
  }

  private def getEmptyCell(emptyCellCount: Int): (Int, Int) = {
    var cellCount = emptyCellCount

    for (row <- 0 until Board.DIM) {
      for (column <- 0 until Board.DIM) {
        if (cells(row)(column) == Board.CellState.Empty) {
          if (cellCount == 0) {
            return (row, column)
          }
          cellCount -= 1
        }
      }
    }
    null
  }

  def getTrapMoveNoCenter(state: Board.CellState, rowA: Int, columnA: Int): (Int, Int) = {
    if (cells(rowA)(columnA) != state) {
      return null
    }
    val (rowB, rowC) = rowA match {
      case 0 => (1, 2)
      case 1 => (0, 2)
      case 2 => (0, 1)
    }
    val (columnB, columnC) = columnA match {
      case 0 => (1, 2)
      case 1 => (0, 2)
      case 2 => (0, 1)
    }
    if (((cells(rowA)(columnB) == state && cells(rowA)(columnC) == Board.CellState.Empty) ||
        (cells(rowA)(columnC) == state && cells(rowA)(columnB) == Board.CellState.Empty)) &&
        cells(rowB)(columnA) == Board.CellState.Empty &&
        cells(rowC)(columnA) == Board.CellState.Empty) {
      return (rowB, columnA)
    }
    if (((cells(rowB)(columnA) == state && cells(rowC)(columnA) == Board.CellState.Empty) ||
        (cells(rowC)(columnA) == state && cells(rowB)(columnA) == Board.CellState.Empty)) &&
        cells(rowA)(columnB) == Board.CellState.Empty &&
        cells(rowA)(columnC) == Board.CellState.Empty) {
      return (rowA, columnB)
    }
    if (((cells(rowA)(columnB) == state && cells(rowA)(columnC) == Board.CellState.Empty) ||
        (cells(rowA)(columnC) == state && cells(rowA)(columnB) == Board.CellState.Empty)) &&
        cells(rowB)(columnB) == Board.CellState.Empty &&
        cells(rowC)(columnC) == Board.CellState.Empty) {
      return (rowB, columnB)
    }
    if (((cells(rowB)(columnB) == state && cells(2)(columnC) == Board.CellState.Empty) ||
        (cells(rowC)(columnC) == state && cells(rowB)(columnB) == Board.CellState.Empty)) &&
        cells(rowA)(columnB) == Board.CellState.Empty &&
        cells(rowA)(columnC) == Board.CellState.Empty) {
      return (rowA, columnB)
    }
    if (((cells(rowB)(columnA) == state && cells(rowC)(columnA) == Board.CellState.Empty) ||
        (cells(rowC)(columnA) == state && cells(rowB)(columnA) == Board.CellState.Empty)) &&
        cells(rowB)(columnB) == Board.CellState.Empty &&
        cells(rowC)(columnC) == Board.CellState.Empty) {
      return (rowB, columnB)
    }
    if (((cells(rowB)(columnB) == state && cells(rowC)(columnC) == Board.CellState.Empty) ||
        (cells(rowC)(columnC) == state && cells(rowB)(columnB) == Board.CellState.Empty)) &&
        cells(rowB)(columnA) == Board.CellState.Empty &&
        cells(rowC)(columnA) == Board.CellState.Empty) {
      return (rowB, columnA)
    }
    null
  }

  def getTrapMoveCenter(state: Board.CellState): (Int, Int) = {
    null
  }

  def getTrapMove(state: Board.CellState): (Int, Int) = {
    for (row <- 0 until Board.DIM) {
      for (column <- 0 until Board.DIM) {
        if ((row != 2) && (column != 2)) {
          val result = getTrapMoveNoCenter(state, row, column)
          if (result != null) return result
        } else {
          val result = getTrapMoveCenter(state)
          if (result != null) return result
        }
      }
    }
    null
  }

  def getOpponentCornerMove(state: Board.CellState): (Int, Int) = {
    if (countEmptyCells < 8) {
      return null
    }
    if (cells(0)(0) == state) {
      return (1, 1)
    }
    if (cells(0)(2) == state) {
      return (1, 1)
    }
    if (cells(2)(2) == state) {
      return (1, 1)
    }
    if (cells(2)(0) == state) {
      return (1, 1)
    }
    null
  }

  def getCornerMove: (Int, Int) = {
    if (countEmptyCells < 8) {
      return null
    }
    if (cells(0)(0) == Board.CellState.Empty) {
      return (0, 0)
    }
    if (cells(0)(2) == Board.CellState.Empty) {
      return (0, 2)
    }
    if (cells(2)(2) == Board.CellState.Empty) {
      return (2, 2)
    }
    if (cells(2)(0) == Board.CellState.Empty) {
      return (2, 0)
    }
    null
  }

  def getComputerMove: (Int, Int) = {
    var result = getLineMove(Board.CellState.Computer)
    if (result == null) result = getLineMove(Board.CellState.Player)
    if (result == null) result = getTrapMove(Board.CellState.Computer)
    if (result == null) result = getTrapMove(Board.CellState.Player)
    if (result == null) result = getOpponentCornerMove(Board.CellState.Player)
    if (result == null) result = getCornerMove
    if (result == null) result = getEmptyCell(Board.random.nextInt(countEmptyCells))
    result
  }
}
