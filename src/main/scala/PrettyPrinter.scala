import scala.collection.mutable

object PrettyPrinter {

  def printSolution(solution: mutable.Set[ConstraintVar]): Unit = {
//    solution.foreachEntry((k, v) => {
//      println(k.string())
//      v.foreach(t => println(t.string()))
//    })

  solution.foreach(f => {
    println(f.string())
    f.solution.foreach(t => {print(t.string())})
    println()
  })



  }

   def printProgram(program: Program): Unit = {
    program.foreach(f => println(f.print()))
  }

  def printConstraints(solver: Solver): Unit = {
    println("Token Constraints:")
    solver.addrConstraints.foreach(f => println(f))
    println()
    println("Copy Constraints:")
    solver.copyConstraints.foreach(f => println(f))
    println()
    println("Complex Constraints:")
    solver.complexConstraints.foreach(f => println(f))
  }

}