package xyz.hyperreal.stala

import xyz.hyperreal.pattern_matcher.Reader

import scala.collection.immutable.ArraySeq


object Preprocessor {

  def preprocessBlock( block: BlockExpression, scope: List[Map[String, DeclarationAST]] = Nil ): Unit = {
    val map = block.decls.map( d => d.name -> d ).toMap
    val newscope =
      block.decls.groupBy(_.name) collectFirst { case (_, _ :: x :: _) => List(x) } match {
        case None =>
          block.decls foreach {
            case ConstDeclaration(pos, name, value, _) =>
              preprocessExpression(value, scope)
            case VarDeclaration( pos, name, None, _ ) =>
            case VarDeclaration( pos, name, Some(value), _ ) =>
              preprocessExpression(value, scope)
          }
//          val funcs =
//            block.funcs map {
//              case FunctionDeclaration(pos, name, parms, stat) =>
//            }
//            case m@MachineDeclaration( pos, name, decls, states ) =>
//              name -> m
//            case d => d.name -> d
        case Some( List(d) ) => sys.error( d.pos.longErrorText(s"'${d.name}' is already defined") )
      }

    block.stats foreach (preprocessStatement( _, map :: scope ))
  }

  def find( name: String, scope: List[Map[String, DeclarationAST]] ): Option[Any] =
    scope match {
      case Nil => None
      case /*outer@*/h :: t => h get name match {
        case None => find( name, t )
        case a => a //Some( (v, outer) )
      }
    }

  def preprocessStatement( stat: StatementAST, scope: List[Map[String, DeclarationAST]] ): Unit =
    stat match {
      case ExpressionStatement( expr ) => preprocessExpression( expr, scope )
      case g@GotoStatement( pos, name, _ ) =>
        find( name, scope ) match {
          case None => sys.error( pos.longErrorText(s"state '$name' not found") )
          case Some( s: StateDeclaration ) => g.stat = s
          case Some( _ ) => sys.error(pos.longErrorText(s"'$name' not a state"))
        }
    }

  def preprocessExpression( expr: ExpressionAST, scope: List[Map[String, DeclarationAST]] ): Unit = {
//    find( name, scope ) match {
//      case None => sys.error( pos.longErrorText(s"variable '$name' not found") )
//      case Some( v: Var ) => v.v = preprocessAndEvalExpression( expr, scope )
//      case _ => sys.error(pos.longErrorText(s"'$name' not assignable"))
//    }

  }

}