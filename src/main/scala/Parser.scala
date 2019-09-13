package xyz.hyperreal.stala

import xyz.hyperreal.pattern_matcher.{Matchers, Reader}


object Parser extends Matchers[Reader] {

  reserved ++= List( "const", "var", "procedure", "odd", "if", "then", "while", "do", "call" )
  delimiters ++= List( "+", "-", "*", "/", "(", ")", ";", ",", ".", ":=", "=", "#", "<", "<=", ">", ">=", "!", "{", "}" )

  def program = matchall(block <~ ".")

  def const = pos ~ ident ~ "=" ~ integerLit ^^ {
    case p ~ n ~ _ ~ v => n -> (p, v)
  }

  def consts = opt("const" ~> rep1sep(const, ",") <~ ";") ^^ {
    case None => Nil
    case Some( l ) => l
  }

  def vari = pos ~ ident ~ opt("=" ~> integerLit) ^^ {
    case p ~ n ~ None => n -> (p, new Var( 0 ))
    case p ~ n ~ Some( v ) => n -> (p, new Var( v ))
  }

  def vars = opt("var" ~> rep1sep(vari, ",") <~ ";") ^^ {
    case None => Nil
    case Some( l ) => l
  }

  def proc: Matcher[(String, (Reader, Block))] = "procedure" ~ pos ~ ident ~ ";" ~ block ~ ";" ^^ {
    case _ ~ p ~ n ~ _ ~ b ~ _ => n -> (p, b)
  }

  def block = consts ~ vars ~ rep(proc) ~ statement ^^ {
    case c ~ v ~ p ~ s => Block( c ++ v ++ p, s )
  }

  def statement: Matcher[Statement] =
    pos ~ ident ~ ":=" ~ expression ^^ { case p ~ n ~ _ ~ e => Assign( p, n, e ) } |
      "call" ~ pos ~ ident ^^ { case _ ~ p ~ n => Call( p, n ) } |
      "!" ~> expression ^^ Write |
      "begin" ~> rep1sep(statement, ";") <~ "end" ^^ Sequence |
      "if" ~ condition ~ "then" ~ statement ^^ { case _ ~ c ~ _ ~ s => If( c, s ) } |
      "while" ~ condition ~ "do" ~ statement ^^ { case _ ~ c ~ _ ~ s => While( c, s ) }

  def condition =
    "odd" ~> expression ^^ Odd |
      expression ~ ("="|"#"|"<"|"<="|">"|">=") ~ expression ^^ { case l ~ c ~ r => Comparison( l, c, r ) }

  def expression: Matcher[Expression] = opt("+" | "-") ~ term ~ rep(("+" | "-") ~ term) ^^ {
    case (None|Some("+")) ~ t ~ l => (l foldLeft t) { case (x, o ~ y) => Operation( x, o, y ) }
    case _ ~ t ~ l => (l foldLeft (Negate( t ): Expression)) { case (x, o ~ y) => Operation( x, o, y ) }
  }

  def term = factor ~ rep(("*" | "/") ~ factor) ^^ {
    case f ~ l => (l foldLeft f) { case (x, o ~ y) => Operation( x, o, y ) }
  }

  def factor =
    pos ~ ident ^^ { case p ~ v => Ident( p, v ) } |
      integerLit ^^ Number |
      "(" ~> expression <~ ")"

  def run( s: String ) =
    program( Reader.fromString(s) ) match {
      case Match( result, _ ) => evalBlock( result, Nil )
      case m: Mismatch => m.error
    }


}