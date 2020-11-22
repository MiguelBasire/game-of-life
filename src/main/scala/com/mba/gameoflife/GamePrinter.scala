package com.mba.gameoflife

import com.mba.gameoflife.{Alive, Dead, Game}
import cats.Show
import cats.implicits._

object GamePrinter {
  def printGame(game: Game)(implicit s: Show[Game]): Unit = println(game.show)

   implicit val cell: Show[Cell] = Show.show {
    case Cell(Alive) => "o" 
    case Cell(Dead) => " "
  }

  implicit val line: Show[Game.Columns] = Show.show( cs => cs.map( _.show ).mkString(" ") )
  implicit val game: Show[Game] = Show.show( game => game.cells.map( _.show ).mkString("\n") )
}
