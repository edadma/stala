package xyz.hyperreal.stala

object Evaluator {

  def evalBlock( block: BlockExpression, scope: List[Map[String, DeclarationAST]] ) =
    block.decls.groupBy(_.name) collectFirst { case (_, _ :: x :: _) => List(x) } match {
      case None => evalStatements( block.stats, block.decls.map { d => d.name -> d }.toMap :: scope )
      case Some( List(d) ) => sys.error( d.pos.longErrorText(s"'${d.name}' is a duplicate") )
    }

  def find( name: String, scope: List[Map[String, DeclarationAST]] ): Option[(Any, List[Map[String, DeclarationAST]])] =
    scope match {
      case Nil => None
      case outer@h :: t => h get name match {
        case None => find( name, t )
        case Some( v ) => Some( (v, outer) )
      }
    }

  def evalStatements( stats: List[StatementAST], scope: List[Map[String, DeclarationAST]]) = {
    def eval( result: Any, rest: List[StatementAST] ): Any =
      rest match {
        case Nil => result
        case h :: t => eval( evalStatement(h, scope), t )
      }

    eval( (), stats )
  }

  def evalStatement( stat: StatementAST, scope: List[Map[String, DeclarationAST]]): Any =
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

  def evalCondition( expr: ExpressionAST, scope: List[Map[String, DeclarationAST]] ) = evalExpression( expr, scope ).asInstanceOf[Boolean]

  def evalInt( expr: ExpressionAST, scope: List[Map[String, DeclarationAST]] ) = evalExpression( expr, scope ).asInstanceOf[Number].intValue

  def evalExpression( expr: ExpressionAST, scope: List[Map[String, DeclarationAST]] ): Any =
    expr match {
      case b: BlockExpression => evalBlock( b, scope )
      case ApplyExpression( pos, name, args ) =>
        find( name, scope ) match {
          case None => sys.error(pos.longErrorText(s"function '$name' not declared"))
          case Some( (FunctionDeclaration(_, _, parms, stat), outer) ) =>
            if (args.length < parms.length)
              sys.error( s"too few arguments to apply function '$name'" )
            else if (args.length > parms.length)
              sys.error( s"too few arguments to apply function '$name'" )

            evalStatement( stat, ((parms zip args) map {case ((p, n), v) => ConstDeclaration(p, n, v)}) :: outer )
          case _ => sys.error( pos.longErrorText(s"'$name' not a function") )
        }
      case IfExpression( cond, yes, None ) => if (evalCondition(cond, scope)) evalExpression( yes, scope ) else ()
      case IfExpression( cond, yes, Some(no) ) => if (evalCondition(cond, scope)) evalExpression( yes, scope ) else no
      case IdentExpression(pos, name) =>
        find(name, scope) match {
          case None => sys.error(pos.longErrorText(s"'$name' not declared"))
          case Some((Var(v), _)) => v
          case Some((n: Int, _)) => n
          case _ => sys.error(pos.longErrorText(s"'$name' not an integer"))
        }
      case NumberExpression(n) => n
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