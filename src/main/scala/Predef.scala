package xyz.hyperreal.stala

import scala.collection.immutable.ArraySeq


object Predef {

  val map =
    Map(
      "println" -> NativeFunction( 1, (a: ArraySeq[Any]) => println(a.head) )
    )

}