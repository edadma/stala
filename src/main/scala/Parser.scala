package xyz.hyperreal.stala

import xyz.hyperreal.pattern_matcher.{Matchers, Reader}


object Parser extends Matchers[Reader] {

  reserved ++= List( "const", "var", "def", "odd", "if", "then", "while", "do" )
  delimiters ++= List( "+", "-", "*", "/", "(", ")", ";", ",", ".", ":=", "=", "#", "<", "<=", ">", ">=", "!", "{", "}" )

  def program = matchall( programBlock )

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

  def vars = opt("var" ~> rep1sep(vari, ",") <~ ";") ^^ {
    case None => Nil
    case Some( l ) => l
  }

  def parms =
    repsep(pos ~ ident, ",") ^^ (_ map { case p ~ i => (p, i) })

  def function: Matcher[FunctionDeclaration] = ("def" ~> pos) ~ ident ~ ("(" ~> parms <~ ")") ~ ("=" ~> statement) ^^ {
    case p ~ n ~ ps ~ s => FunctionDeclaration( p, n, ps, Nil, s )
  }

  def programBlock = consts ~ vars ~ rep(function) ~ statement ^^ {
    case c ~ v ~ p ~ s => Program( c ++ v ++ p, s )
  }

  def statement: Matcher[StatementAST] =
    pos ~ ident ~ ":=" ~ expression ^^ { case p ~ n ~ _ ~ e => AssignStatement( p, n, e ) } |
    pos ~ ident ~ ("(" ~> repsep(expression, ",") <~ ")") ^^ {
      case p ~ n ~ a => ApplyStatement( p, n, a ) } |
    "!" ~> expression ^^ WriteStatement |
    "{" ~> rep1sep(statement, ";") <~ "}" ^^ SequenceStatement |
    "if" ~ condition ~ "then" ~ statement ^^ { case _ ~ c ~ _ ~ s => IfStatement( c, s ) } |
    "while" ~ condition ~ "do" ~ statement ^^ { case _ ~ c ~ _ ~ s => WhileStatement( c, s ) }

  def condition =
    expression ~ ("="|"#"|"<"|"<="|">"|">=") ~ expression ^^ { case l ~ c ~ r => ComparisonCondition( l, c, r ) }

  def expression: Matcher[ExpressionAST] = opt("+" | "-") ~ term ~ rep(("+" | "-") ~ term) ^^ {
    case (None|Some("+")) ~ t ~ l => (l foldLeft t) { case (x, o ~ y) => BinaryExpression( x, o, y ) }
    case _ ~ t ~ l => (l foldLeft (NegateExpression( t ): ExpressionAST)) { case (x, o ~ y) => BinaryExpression( x, o, y ) }
  }

  def term: Matcher[ExpressionAST] = factor ~ rep(("*" | "/") ~ factor) ^^ {
    case f ~ l => (l foldLeft f) { case (x, o ~ y) => BinaryExpression( x, o, y ) }
  }

  def factor: Matcher[ExpressionAST] =
    pos ~ ident ^^ { case p ~ v => IdentExpression( p, v ) } |
    integerLit ^^ NumberExpression |
    "(" ~> expression <~ ")"

  def run( s: String ) =
    program( Reader.fromString(s) ) match {
      case Match( result, _ ) => evalBlock( result, Nil )
      case m: Mismatch => m.error
    }


}