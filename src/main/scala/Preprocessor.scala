package xyz.hyperreal.stala

import xyz.hyperreal.pattern_matcher.Reader

import scala.collection.immutable.ArraySeq


object Preprocessor {

  def preprocessBlock( block: BlockExpression, scope: List[Map[String, Any]] = Nil ): Unit = {
    val map =
      block.decls.groupBy(_.name) collectFirst { case (_, _ :: x :: _) => List(x) } match {
        case None =>
          block.decls.map {
            case ConstDeclaration( pos, name, value ) => name -> preprocessAndEvalExpression( value, scope )
            case VarDeclaration( pos, name, None ) => name -> Var( 0 )
            case VarDeclaration( pos, name, Some(value) ) => name -> Var( preprocessAndEvalExpression(value, scope) )
            case m@MachineDeclaration( pos, name, decls, states ) =>
              name -> m
            case d => d.name -> d
          }.toMap
        case Some( List(d) ) => sys.error( d.pos.longErrorText(s"'${d.name}' is a duplicate") )
      }

    block.stats foreach (preprocessStatement( _, map :: scope ))
  }

  def find( name: String, scope: List[Map[String, Any]] ): Option[(Any, List[Map[String, Any]])] =
    scope match {
      case Nil => None
      case outer@h :: t => h get name match {
        case None => find( name, t )
        case Some( v ) => Some( (v, outer) )
      }
    }

  def preprocessStatement( stat: StatementAST, scope: List[Map[String, Any]] ): Unit =
    stat match {
      case ExpressionStatement( expr ) => preprocessExpression( expr, scope )
      case g@GotoStatement( pos, name, _ ) => find( name, scope )
    }

  def preprocessExpression( expr: ExpressionAST, scope: List[Map[String, Any]] ): Unit = {

  }

  def preprocessAndEvalExpression( expr: ExpressionAST, scope: List[Map[String, Any]] ) = {
    preprocessExpression( expr, scope )
    Evaluator.evalExpression( expr )
  }

}