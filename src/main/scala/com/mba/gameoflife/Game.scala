package com.mba.gameoflife

import scala.util.Random



sealed trait CellState
case object Alive extends CellState
case object Dead extends CellState

case class Cell(state: CellState) 

object Cell {

  def evolve (cell: Cell, neighbors : Array[Cell]) : Cell = 
  (cell.state,neighbors.count(_.state == Alive)) match {
    case (Alive,2|3) => Cell(Alive) 
    case (Dead,3) => Cell(Alive)
    case _ => Cell(Dead)
  }
}


case class Game(it: Int, cells: Game.Lines) {
  val (numberOfRows, numberOfCols) = Game.dimension(this)
  def cell(row : Int , column: Int) = if (row < 0 || column < 0  || row >= numberOfRows || column >= numberOfCols) Cell(Dead) else cells(row)(column)
}

object Game {

  type Lines = IndexedSeq[Columns]
  type Columns = IndexedSeq[Cell]

  val initial : Game = Game(it = 0, cells =
      IndexedSeq(  
          IndexedSeq( Cell(Dead), Cell(Dead), Cell(Dead), Cell(Dead), Cell(Dead) ),
          IndexedSeq( Cell(Dead), Cell(Dead), Cell(Dead), Cell(Dead), Cell(Dead) ),
          IndexedSeq( Cell(Dead), Cell(Dead), Cell(Dead), Cell(Dead), Cell(Dead) ),
          IndexedSeq( Cell(Dead), Cell(Alive), Cell(Dead), Cell(Dead), Cell(Dead) ),
          IndexedSeq( Cell(Dead), Cell(Dead), Cell(Alive), Cell(Dead), Cell(Dead) ),
          IndexedSeq( Cell(Dead), Cell(Dead), Cell(Dead), Cell(Dead), Cell(Dead) ),
       )
  )


  def random(rows: Int, columns: Int) : Game = Game(0, IndexedSeq.fill(rows)(IndexedSeq.fill(columns)(Cell(if(Random.nextBoolean) Alive else Dead)))) 

  def games(initial: Game) : LazyList[Game] =  initial #:: games(initial).scan(initial)( (g, _) => Game.play(g) ).tail


  type NumberOfLines = Int
  type NumberOfColumns = Int

  def dimension(game : Game) : (NumberOfLines, NumberOfColumns) = (game.cells.size, game.cells.head.size)

  def neighbors(game: Game, row: Int, column: Int) : Array[Cell] = Array(
    game.cell(row - 1,column - 1),
    game.cell(row - 1,column),
    game.cell(row - 1,column + 1),
    game.cell(row,column - 1),
    game.cell(row, column + 1),
    game.cell(row + 1,column - 1),
    game.cell(row + 1,column),
    game.cell(row + 1,column + 1)
  )


  def play(game : Game) : Game = Game( 
        it = game.it + 1, 
        cells = 
          (for( row <- (0 to game.numberOfRows - 1) ) yield {

            (for ( column <-  0 to game.numberOfCols  - 1 )  yield  Cell.evolve(game.cells(row)(column), neighbors(game,row, column)))
           
          })
  )



}



