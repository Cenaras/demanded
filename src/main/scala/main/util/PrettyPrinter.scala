package main.util

import main.constraint.ConstraintVar
import main.program.Program

import scala.collection.mutable

object PrettyPrinter {

  def printSolution(solution: mutable.Set[ConstraintVar]): Unit = {
    solution.foreach(f => {
      println(f)
      f.solution.foreach(t => {
        print(t)
      })
      println()
    })
  }

  def printProgram(program: Program): String = {
    var pretty = ""
    program.getInstructions.foreach(f => pretty += f.print())
    pretty
  }

  //  def printConstraints(solver: Solver): Unit = {
  //    val constraints = solver.getConstraints
  //    println("Token Constraints:")
  //    constraints.addrConstraints.foreach(f => println(f))
  //    println()
  //    println("Copy Constraints:")
  //    constraints.copyConstraints.foreach(f => println(f))
  //    println()
  //    println("Complex Constraints:")
  //    constraints.complexConstraints.foreach(f => println(f))
  //  }

}