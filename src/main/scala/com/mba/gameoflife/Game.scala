package com.mba.gameoflife

import com.mba.gameoflife.CellState.{Alive, Dead}

import scala.util.Random


case class Game(iterationNumber: Int = 0, cells: Array[Array[Cell]]) {

  def neighboursOf(cell: Cell): List[Cell] = cells.find(_.contains(cell)).map { row =>

    val (x, y) = (cells.indexOf(row), row.indexOf(cell))

    def cellAt(x: Int, y: Int): Cell = {
      val indices = cells.indices
      def inRange(index: Int) = index match {
        case i if i < indices.start => indices.last
        case i if i > indices.last => indices.start
        case i => i
      }
      cells(inRange(x))(inRange(y))
    }

    List(
      cellAt(x - 1, y - 1), cellAt(x - 1, y), cellAt(x - 1, y + 1),
      cellAt(x, y - 1), cellAt(x, y + 1),
      cellAt(x + 1, y - 1), cellAt(x + 1, y), cellAt(x + 1, y + 1)
    )

  }.getOrElse(List())

  def nextIteration(): Game = {

    val next = Game(iterationNumber + 1, this.cells.clone())

    val job = for {
      row <- cells.indices
      column <- cells(0).indices
      cell = cells(row)(column)
    } yield {
      ((row, column), cell, neighboursOf(cell))
    }

    job.par.foreach { case ((x, y), c, neighbours) =>
      next.cells(x)(y) = c.become(neighbours)
    }

    next
  }
}

object Game {


  def generateSquare(size: Int): Game = {

    val cells = Array.ofDim[Cell](size, size)

    val rows = 0 until size
    val columns = 0 until size

    for {
      row <- rows
      column <- columns
    } {
      cells(row)(column) = Cell(if (Random.nextBoolean()) CellState.Alive else CellState.Dead)
    }

    Game(iterationNumber = 0, cells)
  }

}


sealed trait CellState

object CellState {

  case object Dead extends CellState

  case object Alive extends CellState

}

case class Cell(state: CellState) {

  def become(neighbours: List[Cell]): Cell = neighbours.count(_.state == Alive) match {
    case 2 => this
    case 3 => this.copy(state = Alive)
    case _ => this.copy(state = Dead)
  }

}

trait GamePrinter {
  def print(game: Game): Unit
}

object ConsoleGamePrinter extends GamePrinter{

  def string(cell: Cell): String = {

    cell.state match {
      case Dead => "_"
      case Alive => "!"
    }

  }


  def print(game: Game): Unit = {
    println(s"iteration ${game.iterationNumber}")
    game.cells.foreach { row =>
      println(row.map(string).mkString("\t"))
    }
  }


}


object Runner extends App {

  val printer = ConsoleGamePrinter

  var game = Game.generateSquare(3)
  printer.print(game)
  1 to 10 foreach { it =>
    game = game.nextIteration()
    printer.print(game)
  }
}