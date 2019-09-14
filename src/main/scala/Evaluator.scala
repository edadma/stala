package xyz.hyperreal.stala

object Evaluator {

//  def evalBlock( block: Block, outer: List[Map[String, Any]]): Unit =
//    block.decls.groupBy(_._1) collectFirst { case (_, _ :: x :: _) => List(x) } match {
//      case None => evalStatement(block.stat, block.decls.map { case (k, (_, v)) => k -> v }.toMap :: outer)
//      case Some(List((name, (pos, _)))) => sys.error(pos.longErrorText(s"'$name' is a duplicate"))
//    }

  def find( name: String, scope: List[Map[String, Any]] ): Option[(Any, List[Map[String, Any]])] =
    scope match {
      case Nil => None
      case inner@h :: t => h get name match {
        case None => find(name, t)
        case Some(v) => Some((v, inner))
      }
    }

  def evalProgram( prog: ProgramAST ) =
    prog match {
      case ProgramAST( decls, stat ) =>
        evalStatement( stat, Nil )
    }

  def evalStatement( stat: StatementAST, scope: List[Map[String, Any]]): Unit =
    stat match {
//      case AssignStatement( pos, name, expr ) =>
//        find( name, scope ) match {
//          case None => sys.error(pos.longErrorText(s"variable '$name' not declared"))
//          case Some((v: Var, _)) => v.v = evalExpression(expr, scope)
//          case _ => sys.error(pos.longErrorText(s"'$name' not assignable"))
//        }
//      case ApplyStatement( pos, name, args ) =>
//        find(name, scope) match {
//          case None => sys.error(pos.longErrorText(s"procedure '$name' not declared"))
//          case Some((b: Block, inner)) => evalStatement(b, inner)
//          case _ => sys.error(pos.longErrorText(s"'$name' not a procedure"))
//        }
      case IfStatement( cond, body ) => if (evalCondition(cond, scope)) evalStatement( body, scope )
      case SequenceStatement( stats ) => stats foreach (evalStatement(_, scope))
      case WhileStatement( cond, body ) => while (evalCondition(cond, scope)) evalStatement( body, scope )
    }

  def evalCondition( cond: ExpressionAST, scope: List[Map[String, Any]] ) =
    cond match {
      case ComparisonExpression( first, rest ) =>
        var l = evalExpression( first, scope )

        def comp( cs: List[(String, ExpressionAST)] ): Boolean =
          cs match {
            case Nil => true
            case (c, e) :: t =>
              val r = evalExpression( e, scope )
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

  def evalExpression( expr: ExpressionAST, scope: List[Map[String, Any]] ): Int =
    expr match {
//      case IfExpression( cond, body ) => if (evalCondition(cond, scope)) evalStatement(body, scope)
      case IdentExpression(pos, name) =>
        find(name, scope) match {
          case None => sys.error(pos.longErrorText(s"'$name' not declared"))
          case Some((Var(v), _)) => v
          case Some((n: Int, _)) => n
          case _ => sys.error(pos.longErrorText(s"'$name' not an integer"))
        }
      case NumberExpression(n) => n.intValue
      case BinaryExpression(left, "+", right) => evalExpression(left, scope) + evalExpression(right, scope)
      case BinaryExpression(left, "-", right) => evalExpression(left, scope) - evalExpression(right, scope)
      case BinaryExpression(left, "*", right) => evalExpression(left, scope) * evalExpression(right, scope)
      case BinaryExpression(left, "/", right) => evalExpression(left, scope) / evalExpression(right, scope)
    }

}

case class Var( var v: Int )