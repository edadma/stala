package xyz.hyperreal.stala

import xyz.hyperreal.pattern_matcher.{Matchers, Reader}

import scala.collection.immutable.ArraySeq


object Parser extends Matchers[Reader] {

  reserved ++= List( "const", "var", "def", "if", "then", "else", "while", "do" )
  delimiters ++= List( "+", "-", "*", "/", "(", ")", ";", ",", "=", "#", "<", "<=", ">", ">=", "{", "}" )

  def program = matchall( block )

  def const = pos ~ ident ~ "=" ~ expression ^^ {
    case p ~ n ~ _ ~ e => ConstDeclaration( p, n, e )
  }

  def consts = opt("const" ~> rep1sep(const, ",") <~ ";") ^^ {
    case None => Nil
    case Some( l ) => l
  }

  def vari = pos ~ ident ~ opt("=" ~> expression) ^^ {
    case p ~ n ~ e => VarDeclaration( p, n, e )
  }

  def vars =
    opt("var" ~> rep1sep(vari, ",") <~ ";") ^^ {
      case None => Nil
      case Some( l ) => l
    }

  def parms = repsep(pos ~ ident, ",") ^^ (_ map { case p ~ i => (p, i) })

  def function: Matcher[FunctionDeclaration] =
    ("def" ~> pos) ~ ident ~ ("(" ~> parms <~ ")") ~ ("=" ~> (("{" ~> block <~ "}") ^^ ExpressionStatement | statement)) ^^ {
      case p ~ n ~ ps ~ s => FunctionDeclaration( p, n, ArraySeq.from(ps), s )
    }

  def block =
    consts ~ vars ~ rep(function) ~ rep(statement) ^^ {
      case c ~ v ~ p ~ s => BlockExpression( c ++ v ++ p, s )
    }

  def compoundStatement: Matcher[StatementAST] =
    "{" ~> block <~ "}" ^^ ExpressionStatement |
    "if" ~ expression ~ "then" ~ statement ~ opt("else" ~> statement) ^^ { case _ ~ c ~ _ ~ s ~ e  => IfStatement( c, s, e ) } |
    "while" ~ expression ~ "do" ~ statement ^^ { case _ ~ c ~ _ ~ s => WhileStatement( c, s ) }

  def simpleStatement: Matcher[StatementAST] =
    pos ~ ident ~ "=" ~ expression ^^ { case p ~ n ~ _ ~ e => AssignStatement( p, n, e ) } |
    expression ^^ ExpressionStatement

  def statement = compoundStatement | simpleStatement <~ ";"

  def expression: Matcher[ExpressionAST] =
    comparison

  def comparison =
    additive ~ rep(("="|"#"|"<"|"<="|">"|">=") ~ additive) ^^ {
      case f ~ Nil => f
      case f ~ r => ComparisonExpression( f, r map {case c ~ e => (c, e)} )
    }

  def additive =
    opt("+" | "-") ~ term ~ rep(("+" | "-") ~ term) ^^ {
      case (None|Some("+")) ~ t ~ l => (l foldLeft t) { case (x, o ~ y) => BinaryExpression( x, o, y ) }
      case _ ~ t ~ l => (l foldLeft (NegateExpression( t ): ExpressionAST)) { case (x, o ~ y) => BinaryExpression( x, o, y ) }
    }

  def term: Matcher[ExpressionAST] =
    factor ~ rep(("*" | "/") ~ factor) ^^ {
      case f ~ l => (l foldLeft f) { case (x, o ~ y) => BinaryExpression( x, o, y ) }
    }

  def factor: Matcher[ExpressionAST] =
    pos ~ ident ~ ("(" ~> repsep(expression, ",") <~ ")") ^^ {
      case p ~ n ~ a => ApplyExpression( p, n, ArraySeq.from(a) ) } |
    pos ~ ident ^^ { case p ~ v => IdentExpression( p, v ) } |
    integerLit ^^ (n => LiteralExpression( n.asInstanceOf[Number] )) |
    floatLit ^^ (n => LiteralExpression( n.asInstanceOf[Number] )) |
    singleStringLit ^^ LiteralExpression |
    ("if" ~> expression <~ "then") ~ expression ~ opt("else" ~> expression) ^^ { case c ~ y ~ n => IfExpression( c, y, n ) } |
    "{" ~> block <~ "}" |
    "(" ~> expression <~ ")"

  def parseProgram( src: io.Source ) =
    program( Reader.fromSource(src) ) match {
      case Match( result, _ ) => result
      case m: Mismatch => m.error
    }

}