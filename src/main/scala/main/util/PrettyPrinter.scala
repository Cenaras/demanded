package main.util

import main.constraint.ConstraintVar
import main.program.Program

import scala.collection.mutable

object PrettyPrinter {

  def printSolution(solution: mutable.Set[ConstraintVar]): String = {
    var pretty = ""


    solution.foreach(f => {
      if (f.solution.nonEmpty) {
        pretty += f.toString + ": "
        f.solution.foreach(t => {
          pretty += t.toString + ", "
        })
        pretty += "\n"
      }
    })
    pretty
  }

  def printProgram(program: Program): String = {
    var pretty = ""
    program.getInstructions.foreach(f => pretty += f.print() + "\n")
    pretty
  }

}