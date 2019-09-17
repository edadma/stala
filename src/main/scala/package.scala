package xyz.hyperreal

import xyz.hyperreal.pattern_matcher.Reader


package object stala {

  def problem( pos: Reader, error: String ) = sys.error( pos.longErrorText(error) )

}