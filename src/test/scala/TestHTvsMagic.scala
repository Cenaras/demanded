import org.scalatest.funsuite.AnyFunSuite

/** Using the magic set transformation provides a demand driven logic program analysis.
 * The literature claims equivalence between Heintze-Tardieu and Magic sets transformations, without ever showing it.
 * Using the "exhaustive.dl" and "transform_program.sh" script, we can obtain a magic sets transformed analysis.
 *
 * We generate random programs (without function calls) and using the small compiler we produce datalog program files
 * and compare the results of the outputs.
 * */
class TestHTvsMagic extends AnyFunSuite{

}
