package xyz.hyperreal.stala

import scala.collection.immutable.ArraySeq

object Evaluator {

  def evalBlock( block: BlockExpression, scope: List[Map[String, Any]] ) =
    block.decls.groupBy(_.name) collectFirst { case (_, _ :: x :: _) => List(x) } match {
      case None => evalStatements( block.stats, block.decls.map {
        case ConstDeclaration( pos, name, value ) => name -> evalExpression( value, scope )
        case VarDeclaration( pos, name, None ) => name -> Var( 0 )
        case VarDeclaration( pos, name, Some(value) ) => name -> Var( evalExpression(value, scope) )
        case d => d.name -> d }.toMap :: scope )
      case Some( List(d) ) => sys.error( d.pos.longErrorText(s"'${d.name}' is a duplicate") )
    }

  def find( name: String, scope: List[Map[String, Any]] ): Option[(Any, List[Map[String, Any]])] =
    scope match {
      case Nil => None
      case outer@h :: t => h get name match {
        case None => find( name, t )
        case Some( v ) => Some( (v, outer) )
      }
    }

  def evalStatements( stats: List[StatementAST], scope: List[Map[String, Any]]) = {
    def eval( result: Any, rest: List[StatementAST] ): Any =
      rest match {
        case Nil => result
        case h :: t => eval( evalStatement(h, scope), t )
      }

    eval( (), stats )
  }

  def evalStatement( stat: StatementAST, scope: List[Map[String, Any]]): Any =
    stat match {
      case AssignStatement( pos, name, expr ) =>
        find( name, scope ) match {
          case None => sys.error(pos.longErrorText(s"variable '$name' not declared"))
          case Some((v: Var, _)) => v.v = evalExpression( expr, scope )
          case _ => sys.error(pos.longErrorText(s"'$name' not assignable"))
        }
//      case ApplyStatement( pos, name, args ) =>
//        find(name, scope) match {
//          case None => sys.error(pos.longErrorText(s"procedure '$name' not declared"))
//          case Some((b: Block, inner)) => evalStatement(b, inner)
//          case _ => sys.error(pos.longErrorText(s"'$name' not a procedure"))
//        }
      case IfStatement( cond, body ) => if (evalCondition(cond, scope)) evalStatement( body, scope )
      case WhileStatement( cond, body ) => while (evalCondition(cond, scope)) evalStatement( body, scope )
      case ExpressionStatement( expr ) => evalExpression( expr, scope )
    }

  def evalCondition( expr: ExpressionAST, scope: List[Map[String, Any]] ) = evalExpression( expr, scope ).asInstanceOf[Boolean]

  def evalInt( expr: ExpressionAST, scope: List[Map[String, Any]] ) = evalExpression( expr, scope ).asInstanceOf[Number].intValue

  def evalArgs( name: String, args: ArraySeq[ExpressionAST], parmc: Int, scope: List[Map[String, Any]] ) = {
    if (args.length < parmc)
      sys.error( s"too few arguments to apply function '$name'" )
    else if (args.length > parmc)
      sys.error( s"too few arguments to apply function '$name'" )

    args map (evalExpression( _, scope ))
  }

  def evalExpression( expr: ExpressionAST, scope: List[Map[String, Any]] ): Any =
    expr match {
      case b: BlockExpression => evalBlock( b, scope )
      case ApplyExpression( pos, name, args ) =>
        find( name, scope ) match {
          case None => sys.error(pos.longErrorText(s"function '$name' not declared"))
          case Some( (FunctionDeclaration(_, _, parms, stat), outer) ) =>
            evalStatement( stat, ((parms zip evalArgs(name, args, parms.length, scope)) map {case ((_, n), v) => n -> v}).toMap :: outer )
          case Some( (NativeFunction(parmc, func), _) ) => func( evalArgs(name, args, parmc, scope) )
          case _ => sys.error( pos.longErrorText(s"'$name' not a function") )
        }
      case IfExpression( cond, yes, None ) => if (evalCondition(cond, scope)) evalExpression( yes, scope ) else ()
      case IfExpression( cond, yes, Some(no) ) => if (evalCondition(cond, scope)) evalExpression( yes, scope ) else no
      case IdentExpression(pos, name) =>
        find( name, scope ) match {
          case None => sys.error(pos.longErrorText(s"'$name' not declared"))
          case Some((Var(v), _)) => v
          case Some((n: Int, _)) => n
          case _ => sys.error(pos.longErrorText(s"'$name' not an integer"))
        }
      case LiteralExpression( v ) => v
      case BinaryExpression(left, "+", right) => evalInt(left, scope) + evalInt(right, scope)
      case BinaryExpression(left, "-", right) => evalInt(left, scope) - evalInt(right, scope)
      case BinaryExpression(left, "*", right) => evalInt(left, scope) * evalInt(right, scope)
      case BinaryExpression(left, "/", right) => evalInt(left, scope) / evalInt(right, scope)
      case ComparisonExpression( first, rest ) =>
        var l = evalInt( first, scope )

        def comp( cs: List[(String, ExpressionAST)] ): Boolean =
          cs match {
            case Nil => true
            case (c, e) :: t =>
              val r = evalInt( e, scope )
              val res =
                c match {
                  case "<" => l < r
                  case ">" => l > r
                  case "=" => l == r
                  case "!=" => l != r
                  case "<=" => l <= r
                  case ">=" => l >= r
                }

              if (res) {
                l = r
                comp( t )
              } else
                false
          }

        comp( rest )
    }

}

case class Var( var v: Any )

case class NativeFunction( parmc: Int, func: ArraySeq[Any] => Any )