package main.constraint

import main.program.*

type ConstraintVariables = mutable.Set[ConstraintVar]

import scala.collection.mutable

object ConstraintGenerator {

  // TODO: This file is starting to become a big mess. There are too many maps, can we combine some or make
  //  some smarter data structures / types instead of this?

  def generate(program: Program): ConstraintEnvironment = {

    // TODO: Merge into a single, let the solvers apply a .filter when solving
    val constraints: mutable.Set[Constraint] = mutable.Set()


    val id2Cvar: mutable.Map[Int, ConstraintVar] = mutable.Map()
    val id2Token: mutable.Map[Int, Token] = mutable.Map()
    val token2Cvar: mutable.Map[(Token, String), ConstraintVar] = mutable.Map()

    // Mapping a function token to its argument and return constraint variable
    val funInfo: mutable.Map[FunToken, (ConstraintVar, ConstraintVar)] = mutable.Map()


    val constraintVars: ConstraintVariables = mutable.Set()

    // TODO: Can we filter/map/reduce/... this instead of foreach and having mutable maps?
    // Iterate all instructions in the program and generate constraints for them
    program.getInstructions.foreach {
      case NewInsn(varId, tokenId) =>
        val cvar = getOrSetCvar(varId, id2Cvar, constraintVars)
        val token = getOrSetObjToken(tokenId, id2Token, token2Cvar, constraintVars)
        constraints += NewConstraint(cvar, token)
      case AssignInsn(leftId, rightId) =>
        val left = getOrSetCvar(leftId, id2Cvar, constraintVars)
        val right = getOrSetCvar(rightId, id2Cvar, constraintVars)
        constraints += CopyConstraint(left, right)
      case LoadInsn(dstId, baseId, field) =>
        val dst = getOrSetCvar(dstId, id2Cvar, constraintVars)
        val base = getOrSetCvar(baseId, id2Cvar, constraintVars)
        constraints += ForallLoadConstraint(dst, base, field)
      case StoreInsn(baseId, field, srcId) =>
        val base = getOrSetCvar(baseId, id2Cvar, constraintVars)
        val src = getOrSetCvar(srcId, id2Cvar, constraintVars)
        constraints += ForallStoreConstraint(base, field, src)
      case NewFunInsn(varId, argId, retVal, tokenId) =>
        val dstCvar = getOrSetCvar(varId, id2Cvar, constraintVars)
        val argCvar = getOrSetCvar(argId, id2Cvar, constraintVars)
        val retValCVar = getOrSetCvar(retVal, id2Cvar, constraintVars)
        val token = getOrSetFunToken(tokenId, id2Token, token2Cvar, constraintVars)
        funInfo += token -> (argCvar, retValCVar) 
        constraints += NewConstraint(dstCvar, token)
      case CallInsn(res, fun, arg) =>
        val resCvar = getOrSetCvar(res, id2Cvar, constraintVars)
        val funCvar = getOrSetCvar(fun, id2Cvar, constraintVars)
        val argCvar = getOrSetCvar(arg, id2Cvar, constraintVars)
        constraints += CallConstraint(resCvar, funCvar, argCvar)
    }

    ConstraintEnvironment(constraints, id2Cvar, id2Token, token2Cvar, funInfo, constraintVars)
  }


  // TODO: Refactor this file - remove as many mutable as possible.

  private def getOrSetCvar(varId: Int, id2Cvar: mutable.Map[Int, ConstraintVar], constraintVars: ConstraintVariables): ConstraintVar = {
    id2Cvar.get(varId) match
      case Some(value) => value
      case None =>
        val cvar = BaseConstraintVar(varId)
        id2Cvar += varId -> cvar
        constraintVars.add(cvar)
        cvar
  }

  /**
   * Adds the token to the tokenId map and generates field constraint variables for it
   *
   * @param t              the token
   * @param tokenId        id of the token
   * @param id2Token       map from id to the token
   * @param token2Cvar     map from (token, field) to constraint var
   * @param constraintVars list of constraint variables
   */
  private def bindNewToken(t: Token, tokenId: Int, id2Token: mutable.Map[Int, Token], token2Cvar: mutable.Map[(Token, String), ConstraintVar], constraintVars: ConstraintVariables): Unit = {
    id2Token += tokenId -> t
    // TODO: Don't hardcode
    for (f <- Array("f", "g"))
      val tokenCvar = FieldConstraintVar(t, f)
      token2Cvar += (t, f) -> tokenCvar
      constraintVars.add(tokenCvar)
  }

  private def getOrSetObjToken(tokenId: Int, id2Token: mutable.Map[Int, Token], token2Cvar: mutable.Map[(Token, String), ConstraintVar], constraintVars: ConstraintVariables): Token = {
    id2Token.get(tokenId) match
      case Some(value) =>
        value match
          case a: ObjToken => a
          case _ => throw Error("Expected an object token for value %s with id %d".format(value, tokenId))
      case None =>
        val token = ObjToken(tokenId)
        id2Token += tokenId -> token
        bindNewToken(token, tokenId, id2Token, token2Cvar, constraintVars)
        token
  }

  private def getOrSetFunToken(tokenId: Int, id2Token: mutable.Map[Int, Token], token2Cvar: mutable.Map[(Token, String), ConstraintVar], constraintVars: ConstraintVariables): FunToken = {
    id2Token.get(tokenId) match
      case Some(value) =>
        value match
          case a: FunToken => a
          case _ => throw Error("Expected a function token for value %s of id %d".format(value, tokenId))

      case None =>
        val token = FunToken(tokenId)
        bindNewToken(token, tokenId, id2Token, token2Cvar, constraintVars)
        token
  }

}
