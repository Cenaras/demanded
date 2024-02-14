package main.constraint

import main.program.*

type ConstraintVariables = mutable.Set[ConstraintVar]

import scala.collection.mutable

object ConstraintGenerator {

  def generate(program: Program): Constraints = {

    val addrConstraints: mutable.Set[AddrConstraint] = mutable.Set()
    val copyConstraints: mutable.Set[CopyConstraint] = mutable.Set()
    val complexConstraints: mutable.Set[ComplexConstraint] = mutable.Set()

    val id2Cvar: mutable.Map[Int, ConstraintVar] = mutable.Map()
    val id2Token: mutable.Map[Int, Token] = mutable.Map()
    val token2Cvar: mutable.Map[(Token, String), ConstraintVar] = mutable.Map()

    val constraintVars: ConstraintVariables = mutable.Set()

    // TODO: Can we filter/map/reduce/... this instead of foreach and having mutable maps?
    // Iterate all instructions in the program and generate constraints for them
    program.getInstructions.foreach {
      case NewInsn(varId, tokenId) =>
        val cvar = getOrSetCvar(varId, id2Cvar, constraintVars)
        val token = getOrSetToken(tokenId, id2Token, token2Cvar, constraintVars)
        addrConstraints += AddrConstraint(cvar, token)
      case AssignInsn(leftId, rightId) =>
        val left = getOrSetCvar(leftId, id2Cvar, constraintVars)
        val right = getOrSetCvar(rightId, id2Cvar, constraintVars)
        copyConstraints += CopyConstraint(left, right)
      case LoadInsn(dstId, baseId, field) =>
        val dst = getOrSetCvar(dstId, id2Cvar, constraintVars)
        val base = getOrSetCvar(baseId, id2Cvar, constraintVars)
        complexConstraints += ForallLoadConstraint(dst, base, field)
      case StoreInsn(baseId, field, srcId) =>
        val base = getOrSetCvar(baseId, id2Cvar, constraintVars)
        val src = getOrSetCvar(srcId, id2Cvar, constraintVars)
        complexConstraints += ForallStoreConstraint(base, field, src)
    }

    Constraints(addrConstraints, copyConstraints, complexConstraints, id2Cvar, id2Token, token2Cvar, constraintVars)


  }


  private def getOrSetCvar(varId: Int, id2Cvar: mutable.Map[Int, ConstraintVar], constraintVars: ConstraintVariables): ConstraintVar = {
    id2Cvar.get(varId) match
      case Some(value) => value
      case None =>
        val cvar = BaseConstraintVar(varId)
        id2Cvar += varId -> cvar
        constraintVars.add(cvar)
        cvar
  }

  private def getOrSetToken(tokenId: Int, id2Token: mutable.Map[Int, Token], token2Cvar: mutable.Map[(Token, String), ConstraintVar], constraintVars: ConstraintVariables): Token = {
    id2Token.get(tokenId) match
      case Some(value) => value
      case None =>
        val token = Token(tokenId)
        id2Token += tokenId -> token
        // TODO: Don't hardcode
        for (f <- Array("f", "g"))
          val tokenCvar = FieldConstraintVar(token, f)
          token2Cvar += (token, f) -> tokenCvar
          constraintVars.add(tokenCvar)
        token
  }

}
