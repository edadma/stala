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
      |c = 123;
      |println( c )
    """.stripMargin
  val ast = Parser.parseProgram( io.Source.fromString(s) )

  println( ast )

  Evaluator.evalBlock( ast, List(Predef.map) )

}