package xyz.hyperreal.stala

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer

object Evaluator {

  def evalBlock( block: BlockExpression ) = {
    block.decls foreach {
      case c@ConstDeclaration( pos, name, init, _ ) => c.value = evalExpression( init )
      case VarDeclaration( pos, name, None, _ ) =>
      case v@VarDeclaration( pos, name, Some(init), _ ) => v.value = evalExpression( init )
      case m@MachineDeclaration( pos, name, decls, states ) =>
    }

    evalStatements( block.stats )
  }

  def evalStatements( stats: List[StatementAST] ) = {
    def eval( result: Any, rest: List[StatementAST] ): Any =
      rest match {
        case Nil => result
        case h :: t => eval( evalStatement(h), t )
      }

    eval( (), stats )
  }

  def evalStatement( stat: StatementAST): Any =
    stat match {
      case AssignStatement( pos, name, expr, decl ) => decl.value = evalExpression( expr )
//      case ApplyStatement( pos, name, args ) =>
//        find(name, scope) match {
//          case None => problem( pos, s"procedure '$name' not declared"))
//          case Some((b: Block, inner)) => evalStatement(b, inner)
//          case _ => problem( pos, s"'$name' not a procedure"))
//        }
      case IfStatement( cond, stat, els ) =>
        if (evalCondition( cond ))
          evalStatement( stat )
        else if (els isDefined)
          evalStatement( els.get )
      case ForStatement( idx, pos, expr, stat, const ) =>
        val it =
          evalExpression( expr ) match {
            case e: IterableOnce[Any] =>
              for (i <- e.iterator) {
                const.value = i
                evalStatement( stat )
              }
            case _ => problem( pos, s"not iterable" )
          }
      case WhileStatement( cond, body ) => while (evalCondition( cond )) evalStatement( body )
      case ExpressionStatement( expr ) => evalExpression( expr )
    }

  def evalCondition( expr: ExpressionAST ) = evalExpression( expr ).asInstanceOf[Boolean]

  def evalInt( expr: ExpressionAST ) = evalExpression( expr ).asInstanceOf[Number].intValue

  def evalArgs( name: String, args: ArraySeq[ExpressionAST], parmc: Int ) = {
    if (parmc > 0)
      if (args.length < parmc)
        sys.error( s"too few arguments to apply function '$name'" )
      else if (args.length > parmc)
        sys.error( s"too few arguments to apply function '$name'" )

    args map (evalExpression( _ ))
  }

  def evalExpression( expr: ExpressionAST ): Any =
    expr match {
      case ListExpression( list ) => list map evalExpression
      case b: BlockExpression => evalBlock( b )
      case ApplyExpression( pos, name, args, decl ) =>
        decl.value match {
//          case FunctionDeclaration( _, _, parms, stat ) =>
//            evalStatement( stat, ((parms zip evalArgs(name, args, parms.length)) map {case ((_, n), v) => n -> v}).toMap :: outer )
          case NativeFunction( parmc, func ) =>
            func( evalArgs(name, args, parmc) )
          case _ => problem( pos, s"'$name' not a function" )
        }
      case IfExpression( cond, yes, None ) => if (evalCondition(cond)) evalExpression( yes ) else ()
      case IfExpression( cond, yes, Some(no) ) => if (evalCondition(cond)) evalExpression( yes ) else evalExpression( no )
      case IdentExpression( _, _, decl ) => decl.value
      case LiteralExpression( v ) => v
      case RangeExpression( left, right ) => evalInt( left ) to evalInt( right )
      case UnaryExpression( "-", expr ) => -evalInt( expr )
      case BinaryExpression(left, "+", right) => evalInt(left) + evalInt(right)
      case BinaryExpression(left, "-", right) => evalInt(left) - evalInt(right)
      case BinaryExpression(left, "*", right) => evalInt(left) * evalInt(right)
      case BinaryExpression(left, "/", right) => evalInt(left) / evalInt(right)
      case ComparisonExpression( first, rest ) =>
        var l = evalInt( first )

        def comp( cs: List[(String, ExpressionAST)] ): Boolean =
          cs match {
            case Nil => true
            case (c, e) :: t =>
              val r = evalInt( e )
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

case class NativeFunction( parmc: Int, func: ArraySeq[Any] => Any )