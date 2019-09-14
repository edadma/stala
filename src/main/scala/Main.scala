package xyz.hyperreal.stala


object Main extends App {

  val s =
    """
      |!123
    """.stripMargin
  val ast = Parser.parseProgram( io.Source.fromString(s) )

  println( ast )

  Evaluator.evalProgram( ast )

}