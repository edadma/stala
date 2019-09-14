package xyz.hyperreal.stala

object Evaluator {

//  def evalBlock( block: Block, outer: List[Map[String, Any]]): Unit =
//    block.decls.groupBy(_._1) collectFirst { case (_, _ :: x :: _) => List(x) } match {
//      case None => evalStatement(block.stat, block.decls.map { case (k, (_, v)) => k -> v }.toMap :: outer)
//      case Some(List((name, (pos, _)))) => sys.error(pos.longErrorText(s"'$name' is a duplicate"))
//    }

  def find(name: String, scope: List[Map[String, Any]]): Option[(Any, List[Map[String, Any]])] =
    scope match {
      case Nil => None
      case inner@h :: t => h get name match {
        case None => find(name, t)
        case Some(v) => Some((v, inner))
      }
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
      case WriteStatement( expr ) => println(evalExpression(expr, scope))
      case SequenceStatement( stats ) => stats foreach (evalStatement(_, scope))
      case IfStatement( cond, body ) => if (evalCondition(cond, scope)) evalStatement(body, scope)
      case WhileStatement( cond, body ) => while (evalCondition(cond, scope)) evalStatement(body, scope)
    }

  def evalCondition( cond: ConditionAST, scope: List[Map[String, Any]] ) =
    cond match {
      case ComparisonCondition(left, "<", right) => evalExpression(left, scope) < evalExpression(right, scope)
      case ComparisonCondition(left, ">", right) => evalExpression(left, scope) > evalExpression(right, scope)
      case ComparisonCondition(left, "=", right) => evalExpression(left, scope) == evalExpression(right, scope)
      case ComparisonCondition(left, "#", right) => evalExpression(left, scope) != evalExpression(right, scope)
      case ComparisonCondition(left, "<=", right) => evalExpression(left, scope) <= evalExpression(right, scope)
      case ComparisonCondition(left, ">=", right) => evalExpression(left, scope) >= evalExpression(right, scope)
    }

  def evalExpression(expr: ExpressionAST, scope: List[Map[String, Any]]): Int =
    expr match {
      case IdentExpression(pos, name) =>
        find(name, scope) match {
          case None => sys.error(pos.longErrorText(s"'$name' not declared"))
          case Some((Var(v), _)) => v
          case Some((n: Int, _)) => n
          case _ => sys.error(pos.longErrorText(s"'$name' not an integer"))
        }
      case NumberExpression(n) => n
      case BinaryExpression(left, "+", right) => evalExpression(left, scope) + evalExpression(right, scope)
      case BinaryExpression(left, "-", right) => evalExpression(left, scope) - evalExpression(right, scope)
      case BinaryExpression(left, "*", right) => evalExpression(left, scope) * evalExpression(right, scope)
      case BinaryExpression(left, "/", right) => evalExpression(left, scope) / evalExpression(right, scope)
    }

}

case class Var( var v: Int )