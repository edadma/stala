package xyz.hyperreal.stala

import scala.collection.immutable.ArraySeq


object Main extends App {

  val s =
    """
      |const a = 3, b = 4;
      |var c = 3 + 4;
      |
      |c = 5 + a;
      |println( c );
      |c = 123;
      |println( c )
    """.stripMargin
  val ast = Parser.parseProgram( io.Source.fromString(s) )

  println( ast )

  val predefs =
    Map(
      "println" -> NativeFunction( 1, (a: ArraySeq[Any]) => println(a.head) )
    )

  Evaluator.evalBlock( ast, List(predefs) )

}