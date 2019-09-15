package xyz.hyperreal.stala

import xyz.hyperreal.pattern_matcher.Reader

import scala.collection.immutable.ArraySeq


object Preprocessor {

  def preprocessBlock( block: BlockExpression, scope: List[Map[String, Any]] = Nil ): Unit = {
    val map =
      (block.consts ++ block.vars ++ block.funcs ++ block.machs).groupBy(_.name) collectFirst { case (_, _ :: x :: _) => List(x) } match {
        case None =>
          val consts =
            block.consts map {
              case ConstDeclaration(pos, name, value) => name -> preprocessExpression(value, scope)
            }
          val vars =
            block.vars map {
              case VarDeclaration( pos, name, None ) => name -> Var( 0 )
              case VarDeclaration( pos, name, Some(value) ) => name -> Var( preprocessExpression(value, scope) )
            }
          val funcs =
            block.funcs map {
              case FunctionDeclaration( pos,)
            case m@MachineDeclaration( pos, name, decls, states ) =>
              name -> m
            case d => d.name -> d
          }.toMap
        case Some( List(d) ) => sys.error( d.pos.longErrorText(s"'${d.name}' is already defined") )
      }

    block.stats foreach (preprocessStatement( _, map :: scope ))
  }

  def find( name: String, scope: List[Map[String, Any]] ): Option[Any] =
    scope match {
      case Nil => None
      case /*outer@*/h :: t => h get name match {
        case None => find( name, t )
        case a => a //Some( (v, outer) )
      }
    }

  def preprocessStatement( stat: StatementAST, scope: List[Map[String, Any]] ): Unit =
    stat match {
      case ExpressionStatement( expr ) => preprocessExpression( expr, scope )
      case g@GotoStatement( pos, name, _ ) =>
        find( name, scope ) match {
          case None => sys.error( pos.longErrorText(s"variable '$name' not found") )
          case Some( v: Var ) => v.v = preprocessAndEvalExpression( expr, scope )
          case _ => sys.error(pos.longErrorText(s"'$name' not assignable"))
        }
    }

  def preprocessExpression( expr: ExpressionAST, scope: List[Map[String, Any]] ): Unit = {

  }

  def preprocessAndEvalExpression( expr: ExpressionAST, scope: List[Map[String, Any]] ) = {
    preprocessExpression( expr, scope )
    Evaluator.evalExpression( expr )
  }

}