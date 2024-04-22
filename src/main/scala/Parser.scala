import scala.collection.mutable.ArrayBuffer

object Parser {

  def ParseProgram(program: String): Program = {
    val newPattern = """x([0-9]+) = new t([0-9]+)""".r
    val assignPattern = """x([0-9]+) = x([0-9]+)""".r
    val loadPattern = """x([0-9]+) = x([0-9]+).f([0-9]+)""".r
    val storePattern = """x([0-9]+).f([0-9]+) = x([0-9]+)""".r

    val lines = program.split("\n")

    val insn = lines.foldLeft(List[Instruction]())((acc, line) => {
      line match
        case newPattern(x, t) => New(x.toInt, t.toInt) :: acc
        case assignPattern(left, right) => Assign(left.toInt, right.toInt) :: acc
        case loadPattern(left, right, field) => Load(left.toInt, right.toInt, field.toInt) :: acc
        case storePattern(left, field, right) => Store(left.toInt, field.toInt, right.toInt) :: acc

    })
    Program(insn)
  }


  def ParseTemplate(name: String): Program = {

    def readFile(path: String): String = {
      val source = scala.io.Source.fromFile(path)
      val lines = try source.getLines().map(_.trim).mkString("\n")
      finally source.close()
      lines
    }

    ParseProgram(readFile("untitled/src/main/scala/programs/" + name))
  }

}
