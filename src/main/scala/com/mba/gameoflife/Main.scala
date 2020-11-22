package com.mba.gameoflife

import com.mba.gameoflife.Game
import com.mba.gameoflife.GamePrinter._

object Main extends App {

  val gs =  Game.games(Game.random(72,75))


  println(" first ###### ")
  printGame(gs.head)
  

  gs.zip(gs.drop(1))
  .takeWhile{  case (g, next) => g.cells != next.cells }
  .map( _._2 ).foreach { g => 
    Thread.sleep(500)
    println(s"############ ${g.it}")  
    printGame(g)(game) 

  }


}