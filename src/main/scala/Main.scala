package xyz.hyperreal.stala


object Main extends App {

  val s =
    """
      |const a = 3, b = 4;
      |var c = 3 + 4;
      |
      |def func( a ) = a + 5;
      |
      |def f( a, b ) = {
      |  var i = a;
      |
      |  while i <= b do {
      |    println( i );
      |    i = i + 1;
      |  }
      |
      |  234;
      |}
      |
      |machine nice {
      |  state letter_n {
      |    on 'n' do goto letter_i;
      |    otherwise { println( 'expected n' ); }
      |  }
      |
      |  state letter_i {
      |    on 'i' do goto letter_c;
      |    otherwise { println( 'expected i' ); }
      |  }
      |
      |  state letter_c {
      |    on 'c' do goto letter_e;
      |    otherwise { println( 'expected c' ); }
      |  }
      |
      |  state letter_e {
      |    on 'e' do println( 'nice!' );
      |    otherwise { println( 'expected e' ); }
      |  }
      |}
      |
      |println( func(6) );
      |println( f(3, 5) );
      |c = 5 + a;
      |println( c );
      |c = 300;
      |println( c );
      |
      |if c = 300 then println( 'equal' ); else println( 'not' );
      |
      |c = 0;
      |
      |while c < 5 do {
      |  c = c + 1;
      |  println( c );
      |}
      |
      |println( 'done' );
    """.stripMargin
  val ast = Parser.parseProgram( io.Source.fromString(s) )

  println( ast )
  Preprocessor.preprocessBlock( ast.decls, ast.stats, Predef.map )
  println( ast )
  Evaluator.evalBlock( ast )

}