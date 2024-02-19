package main

import main.program.{ProgramDistribution, ProgramGenerator}
import main.util.PrettyPrinter

object Main {


  @main def run(): Unit = {
    val d = ProgramDistribution(20, 25, 10, 10, 20, 15)
    val g = new ProgramGenerator(5, 3, 10, d)
    val p = g.generate()
    println(PrettyPrinter.stringifyProgram(p))

  }

}
