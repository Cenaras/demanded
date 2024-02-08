package main

import main.program.ProgramGenerator
import main.solver.ExhaustiveSolver
import main.util.PrettyPrinter

object Main {


  @main def run(): Unit = {
    val varNumber = 4
    val tokenNum = 2
    val programSize = 8
    val generator: ProgramGenerator = new ProgramGenerator(varNumber, tokenNum, programSize);
    val program = generator.generate()

//    println("Generated Program:")
//    PrettyPrinter.printProgram(program)
//
//
//    val solver = new ExhaustiveSolver(program)
//    solver.generateConstraints()
//    
//    PrettyPrinter.printConstraints(solver)
//    val solution = solver.solve()
//    PrettyPrinter.printSolution(solution)
  }

}
