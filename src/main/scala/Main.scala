package xyz.hyperreal.stala

import scala.collection.immutable.ArraySeq


object Main extends App {

  val s =
    """
      |println( 123 )
    """.stripMargin
  val ast = Parser.parseProgram( io.Source.fromString(s) )

  println( ast )

  val predefs =
    Map(
      "println" -> NativeFunction( 1, (a: ArraySeq[Any]) => println(a.head) )
    )

  Evaluator.evalBlock( ast, List(predefs) )

}