package xyz.hyperreal.stala

import xyz.hyperreal.pattern_matcher.Reader

import scala.collection.immutable.ArraySeq


object Preprocessor {

  def preprocessBlock( decls: List[DeclarationAST], stats: List[StatementAST], scope: List[Map[String, DeclarationAST]] ): Unit = {
    val map = decls.map( d => d.name -> d ).toMap
    val newscope = map :: scope

    decls.groupBy(_.name) collectFirst { case (_, _ :: x :: _) => List(x) } match {
      case None =>
        decls foreach {
          case ConstDeclaration(pos, name, value, _) =>
            preprocessExpression(value, newscope)
          case VarDeclaration( pos, name, None, _ ) =>
          case VarDeclaration( pos, name, Some(value), _ ) =>
            preprocessExpression(value, newscope)
        }
//          val funcs =
//            block.funcs map {
//              case FunctionDeclaration(pos, name, parms, stat) =>
//            }
//            case m@MachineDeclaration( pos, name, decls, states ) =>
//              name -> m
//            case d => d.name -> d
      case Some( List(d) ) => problem( d.pos, s"'${d.name}' is already defined" )
    }

    stats foreach (preprocessStatement( _, newscope ))
  }

  def find( name: String, scope: List[Map[String, DeclarationAST]] ): Option[DeclarationAST] =
    scope match {
      case Nil => None
      case /*outer@*/h :: t => h get name match {
        case None => find( name, t )
        case a => a //Some( (v, outer) )
      }
    }

  def preprocessStatement( stat: StatementAST, scope: List[Map[String, DeclarationAST]] ): Unit =
    stat match {
      case a@AssignStatement( pos, name, expr, _ ) =>
        find( name, scope ) match {
          case None => problem( pos, s"variable '$name' not found" )
          case Some( v: VarDeclaration ) => a.decl = v
          case Some( _ ) => problem( pos, s"'$name' not a variable" )
        }
      case ExpressionStatement( expr ) => preprocessExpression( expr, scope )
      case g@GotoStatement( pos, name, _ ) =>
        find( name, scope ) match {
          case None => problem( pos, s"state '$name' not found" )
          case Some( s: StateDeclaration ) => g.stat = s
          case Some( _ ) => problem( pos, s"'$name' not a state" )
        }
    }

  def preprocessExpression( expr: ExpressionAST, scope: List[Map[String, DeclarationAST]] ): Unit =
    expr match {
      case BlockExpression( decls, stats ) => preprocessBlock( decls, stats, scope )
      case _: LiteralExpression =>
      case a@ApplyExpression( pos, name, args, _ ) =>
        find( name, scope ) match {
          case None => problem( pos, s"function '$name' not found" )
          case Some( d@(_: ConstDeclaration | _: VarDeclaration | _:FunctionDeclaration) ) => a.decl = d
          case Some( d ) => problem( pos, s"'${d.name}' not found a function" )
        }

        args foreach (preprocessExpression( _, scope ))
      case id@IdentExpression( pos, name, _ ) =>
        find( name, scope ) match {
          case None => problem( pos, s"'$name' not found" )
          case Some( d ) => id.decl = d
        }
    }

}