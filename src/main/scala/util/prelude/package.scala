package util

/**
  * Created by i on 2/10/16.
  */
package object prelude {
  def id [A]: A => A = x => x

  implicit class EnrichedFunction[-A,+B](f: A => B) {

    /**
      * Shortcut for composition
      * @return a composition of functions
      */
    def o [R] (g: R => A): R => B = x => f(g(x))

  }
}



object Test {
  def main(args: Array[String]): Unit = {
    val f: Int => Int = (x: Int) => x + 3
    val g: Int => Int = (x: Int) => x * 3

    println( f compose g apply(3))

    import prelude._
    println( f o g apply (3))
  }
}
