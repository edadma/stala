package xyz.hyperreal.stala


object Main extends App {

  val s =
    """
      |const a = 3, b = 4;
      |var c = 3 + 4;
      |
      |def func( a ) = a + 5;
      |
      |println( func(6) );
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

  Evaluator.evalBlock( ast, List(Predef.map) )

}