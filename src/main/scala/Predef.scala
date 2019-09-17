package xyz.hyperreal.stala

import scala.collection.immutable.ArraySeq


object Predef {

  val list =
    List(
      "println" -> NativeFunction( 1, (a: ArraySeq[Any]) => println(a.head) )
    )

  def map = List( list map { case (k, v) => k -> ConstDeclaration( null, k, null, v ) } toMap )

}