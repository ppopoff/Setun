package util.annotations

import scala.annotation.StaticAnnotation

/**
  * An annotation that denotes that computation
  * introduces a side-effect
  */
class SideEffect extends StaticAnnotation
