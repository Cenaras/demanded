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
    val id2ObjToken: mutable.Map[Int, ObjToken] = mutable.Map()
    val id2FunToken: mutable.Map[Int, FunToken] = mutable.Map()
    val token2Cvar: mutable.Map[(Token, String), ConstraintVar] = mutable.Map()

    // Mapping a function token to its argument and return constraint variable
    val funInfo: mutable.Map[FunToken, (ConstraintVar, ConstraintVar)] = mutable.Map()


    val constraintVars: ConstraintVariables = mutable.Set()

    // TODO: Can we filter/map/reduce/... this instead of foreach and having mutable maps?
    // Iterate all instructions in the program and generate constraints for them
    program.getInstructions.foreach {
      case NewInsn(varId, tokenId) =>
        val cvar = getOrSetCvar(varId, id2Cvar, constraintVars)
        val token = getOrSetObjToken(tokenId, id2ObjToken, token2Cvar, constraintVars)
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
      case NewFunInsn(varId, argId, tokenId) =>
        val dstCvar = getOrSetCvar(varId, id2Cvar, constraintVars)
        val argCvar = getOrSetCvar(argId, id2Cvar, constraintVars)
        val token = getOrSetFunToken(tokenId, id2FunToken, token2Cvar, constraintVars)
        funInfo += token -> (argCvar, argCvar) // FIXME: For now same since always identity function
        constraints += NewConstraint(dstCvar, token)
      case CallInsn(res, fun, arg) =>
        val resCvar = getOrSetCvar(res, id2Cvar, constraintVars)
        val funCvar = getOrSetCvar(fun, id2Cvar, constraintVars)
        val argCvar = getOrSetCvar(arg, id2Cvar, constraintVars)
        constraints += CallConstraint(resCvar, funCvar, argCvar)
    }

    ConstraintEnvironment(constraints, id2Cvar, id2ObjToken, id2FunToken, token2Cvar, funInfo, constraintVars)
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

  private def getOrSetObjToken(tokenId: Int, id2ObjToken: mutable.Map[Int, ObjToken], token2Cvar: mutable.Map[(Token, String), ConstraintVar], constraintVars: ConstraintVariables): Token = {
    id2ObjToken.get(tokenId) match
      case Some(value) => value
      case None =>
        val token = ObjToken(tokenId)
        id2ObjToken += tokenId -> token
        // TODO: Don't hardcode
        for (f <- Array("f", "g"))
          val tokenCvar = FieldConstraintVar(token, f)
          token2Cvar += (token, f) -> tokenCvar
          constraintVars.add(tokenCvar)
        token
  }

  // TODO: Duplicate...
  private def getOrSetFunToken(tokenId: Int, id2FunToken: mutable.Map[Int, FunToken], token2Cvar: mutable.Map[(Token, String), ConstraintVar], constraintVars: ConstraintVariables): FunToken = {
    id2FunToken.get(tokenId) match
      case Some(value) => value
      case None =>
        val token = FunToken(tokenId)
        id2FunToken += tokenId -> token

        for (f <- Array("f", "g"))
          val tokenCvar = FieldConstraintVar(token, f)
          token2Cvar += (token, f) -> tokenCvar
          constraintVars.add(tokenCvar)
        token
  }

}
