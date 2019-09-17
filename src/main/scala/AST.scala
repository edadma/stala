package xyz.hyperreal.stala

import xyz.hyperreal.pattern_matcher.Reader

import scala.collection.immutable.ArraySeq


abstract class AST

abstract class DeclarationAST extends AST {
  val pos: Reader
  val name: String

  def value: Any = this

  def value_=( v: Any ): Unit = sys.error( "bad" )
}

case class FunctionDeclaration( pos: Reader, name: String, parms: ArraySeq[(Reader, String)], stat: StatementAST ) extends DeclarationAST
case class ConstDeclaration( pos: Reader, name: String, init: ExpressionAST, override var value: Any = null ) extends DeclarationAST
case class VarDeclaration( pos: Reader, name: String, init: Option[ExpressionAST], override var value: Any = 0 ) extends DeclarationAST
case class MachineDeclaration( pos: Reader, name: String, decls: List[DeclarationAST], states: List[StateDeclaration] ) extends DeclarationAST
case class StateDeclaration( pos: Reader, name: String, entry: Option[StatementAST], events: List[(ExpressionAST, StatementAST)], default: Option[StatementAST], exit: Option[StatementAST] ) extends DeclarationAST

abstract class StatementAST extends AST
case class GotoStatement( pos: Reader, name: String, var stat: StateDeclaration = null ) extends StatementAST
case class AssignStatement( pos: Reader, name: String, expr: ExpressionAST, var decl: VarDeclaration = null ) extends StatementAST
case class IfStatement( cond: ExpressionAST, stat: StatementAST, els: Option[StatementAST] ) extends StatementAST
case class ForStatement( idx: String, expr: ExpressionAST, stat: StatementAST ) extends StatementAST
case class WhileStatement( cond: ExpressionAST, stat: StatementAST ) extends StatementAST
case class ExpressionStatement( expr: ExpressionAST ) extends StatementAST

abstract class ExpressionAST extends AST
case class IfExpression( cond: ExpressionAST, yes: ExpressionAST, no: Option[ExpressionAST] ) extends ExpressionAST
case class BlockExpression( decls: List[DeclarationAST], stats: List[StatementAST] ) extends ExpressionAST
case class ComparisonExpression( first: ExpressionAST, rest: List[(String, ExpressionAST)] ) extends ExpressionAST
case class ApplyExpression( pos: Reader, name: String, args: ArraySeq[ExpressionAST], var decl: DeclarationAST = null ) extends ExpressionAST
case class UnaryExpression( op: String, expr: ExpressionAST ) extends ExpressionAST
case class BinaryExpression( left: ExpressionAST, op: String, right: ExpressionAST ) extends ExpressionAST
case class LiteralExpression( v: Any ) extends ExpressionAST
case class IdentExpression( pos: Reader, name: String, var decl: DeclarationAST = null ) extends ExpressionAST
case class RangeExpression( left: ExpressionAST, right: ExpressionAST ) extends ExpressionAST