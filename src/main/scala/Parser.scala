object Parser {

  def ParseProgram(program:String):Program ={
    val newPattern = """x([0-9]+) = new t([0-9]+)""".r
    val assignPattern = """x([0-9]+) = x([0-9]+)""".r
    val loadPattern = """x([0-9]+) = x([0-9]+).f([0-9]+)""".r
    val storePattern = """x([0-9]+).f([0-9]+) = x([0-9]+)""".r

    val insn:List[Instruction]=

      List()

    val lines = program.split("\n")
    lines.

      foreach {
        case newPattern(x, t) =>insn::NewInsn (x.toInt, t.toInt)
        case assignPattern(left, right) =>insn::AssignInsn (left.toInt, right.toInt)
        case loadPattern(left, right, field) =>insn::LoadInsn (left.toInt, right.toInt, field)
        case storePattern(left, field, right) =>insn::StoreInsn (left.toInt, field, right.toInt)
      }

    Program(insn)

  }


}
