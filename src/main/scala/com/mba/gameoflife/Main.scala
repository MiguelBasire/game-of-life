package com.mba.gameoflife

import com.mba.gameoflife.Game
import com.mba.gameoflife.GamePrinter._

object Main extends App {

  val games =  Game.games(Game.random(76,150))


  println(" first ###### ")
  printGame(games.head)
  

  games.zip(games.drop(1))
  .takeWhile{  case (g, next) => g.cells != next.cells }
  .map( _._2 ).foreach { g => 
    println(s"############ ${g.it}")  
    printGame(g)(game) 

  }


}