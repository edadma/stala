package xyz.hyperreal.stala

import xyz.hyperreal.pattern_matcher.Reader


abstract class AST

case class Program( decls: List[DeclarationAST], stat: StatementAST ) extends AST

abstract class DeclarationAST extends AST {
  val pos: Reader
  val name: String
}

case class FunctionDeclaration( pos: Reader, name: String, parms: List[(Reader, String)], decls: List[DeclarationAST], stat: StatementAST ) extends DeclarationAST
case class ConstDeclaration( pos: Reader, name: String, value: ExpressionAST ) extends DeclarationAST
case class VarDeclaration( pos: Reader, name: String, value: Option[ExpressionAST] ) extends DeclarationAST

abstract class StatementAST extends AST
case class AssignStatement( pos: Reader, name: String, expr: ExpressionAST ) extends StatementAST
case class ApplyStatement( pos: Reader, name: String, args: List[ExpressionAST] ) extends StatementAST
case class WriteStatement( expr: ExpressionAST ) extends StatementAST
case class SequenceStatement( stats: List[StatementAST] ) extends StatementAST
case class IfStatement( cond: ConditionAST, stat: StatementAST ) extends StatementAST
case class WhileStatement( cond: ConditionAST, stat: StatementAST ) extends StatementAST

abstract class ConditionAST extends AST
case class ComparisonCondition( left: ExpressionAST, comp: String, right: ExpressionAST ) extends ConditionAST

abstract class ExpressionAST extends AST
case class NegateExpression( x: ExpressionAST ) extends ExpressionAST
case class BinaryExpression( left: ExpressionAST, op: String, right: ExpressionAST ) extends ExpressionAST
case class NumberExpression( n: Int ) extends ExpressionAST
case class IdentExpression( pos: Reader, name: String ) extends ExpressionAST