package xyz.hyperreal.stala

import xyz.hyperreal.pattern_matcher.Reader


abstract class AST

case class ProgramAST( decls: List[DeclarationAST], stats: SequenceStatement ) extends AST

abstract class DeclarationAST extends AST {
  val pos: Reader
  val name: String
}

case class FunctionDeclaration( pos: Reader, name: String, parms: List[(Reader, String)], decls: List[DeclarationAST], stat: StatementAST ) extends DeclarationAST
case class ConstDeclaration( pos: Reader, name: String, value: ExpressionAST ) extends DeclarationAST
case class VarDeclaration( pos: Reader, name: String, value: Option[ExpressionAST] ) extends DeclarationAST

abstract class StatementAST extends AST
case class AssignStatement( pos: Reader, name: String, expr: ExpressionAST ) extends StatementAST
case class SequenceStatement( stats: List[StatementAST] ) extends StatementAST
case class IfStatement( cond: ExpressionAST, stat: StatementAST ) extends StatementAST
case class WhileStatement( cond: ExpressionAST, stat: StatementAST ) extends StatementAST
case class ExpressionStatement( expr: ExpressionAST ) extends StatementAST

abstract class ExpressionAST extends AST
case class ComparisonExpression( first: ExpressionAST, rest: List[(String, ExpressionAST)] ) extends ExpressionAST
case class ApplyExpression( pos: Reader, name: String, args: List[ExpressionAST] ) extends ExpressionAST
case class NegateExpression( x: ExpressionAST ) extends ExpressionAST
case class BinaryExpression( left: ExpressionAST, op: String, right: ExpressionAST ) extends ExpressionAST
case class NumberExpression( n: Number ) extends ExpressionAST
case class StringExpression( s: String ) extends ExpressionAST
case class IdentExpression( pos: Reader, name: String ) extends ExpressionAST