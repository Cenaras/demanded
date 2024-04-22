import main.program.{Parser, ProgramDistribution, ProgramGenerator}
import main.util.PrettyPrinter
import org.scalatest.funsuite.AnyFunSuite

class TestParser extends AnyFunSuite {


  test("Program-to-string-to-program-to-string") {
    val g = new ProgramGenerator(20, 5, 50, ProgramDistribution(20, 50, 20, 10))
    val p = g.generate()

    val pString = PrettyPrinter.stringifyProgram(p)

    val parsed = Parser.ParseProgram(pString)
    val parsedString = PrettyPrinter.stringifyProgram(parsed)

    if (!(pString == parsedString)) {
      throw Error("Pretty printing of program p, and PrettyPrint(Parse(PrettyPrint(p))) did not match:\n%s".format(pString))
    }
  }

}
