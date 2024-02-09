import main.program.{Parser, ProgramGenerator}
import main.util.PrettyPrinter
import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {


  test("Program-to-string-to-program-to-string") {
    val g = new ProgramGenerator(20, 5, 50, (20, 50, 20, 10))
    val p = g.generate()

    val pString = PrettyPrinter.printProgram(p)

    val parsed = Parser.ParseProgram(pString)
    val parsedString = PrettyPrinter.printProgram(parsed)

    if (!(pString == parsedString)) {
      throw Error("Pretty printing of program p, and PrettyPrint(Parse(PrettyPrint(p))) did not match:\n%s".format(pString))
    }
  }

}
