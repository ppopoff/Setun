package setun

/**
  * This package contains Ternary class that
  * contains Ternary logic and supported
  * operations
  */
package object ternary {

  sealed trait Ternary
  case object True  extends Ternary
  case object False extends Ternary
  case object Maybe extends Ternary

  def unary_!(t: Ternary) = not(t)
  def ¬ (t: Ternary) = not(t)
  def ait(t: Ternary) = not(t)
  def negate(t: Ternary) = not(t)

  def not (t: Ternary) = t match {
    case True  => False
    case Maybe => Maybe
    case False => True
  }


  implicit class TernaryOps(t: Ternary) {
    def && (other: Ternary) = t and other
    def || (other: Ternary) = t or other

    def ∧ (other: Ternary) = t and other
    def ∨ (other: Ternary) = t or other
    def ≡ (other: Ternary) = t equiv other
    def ⊃ (other: Ternary) = t imply other

    def and (other: Ternary) = (t, other) match {
      case (True, True)   => True
      case (True, Maybe)  => Maybe
      case (True, False)  => False
      case (False, False) => False
      case (False, Maybe) => False
      case (False, True)  => False
      case (Maybe, Maybe) => Maybe
      case (Maybe, True)  => Maybe
      case (Maybe, False) => False
    }

    def or (other: Ternary) = (t, other) match {
      case (True, True) => True
      case (True, Maybe) => True
      case (True, False) => True
      case (Maybe, True)  => True
      case (Maybe, Maybe) => Maybe
      case (Maybe, False) => Maybe
      case (False, True)  => True
      case (False, Maybe) => Maybe
      case (False, False) => False
    }

    def equiv(other: Ternary) = (t, other) match {
      case (True, True)   => True
      case (True, Maybe)  => Maybe
      case (True, False)  => False
      case (Maybe, True)  => Maybe
      case (Maybe, Maybe) => Maybe
      case (Maybe, False) => Maybe
      case (False, True)  => False
      case (False, Maybe) => Maybe
      case (False, False) => True
    }


    def imply(other: Ternary) = (t, other) match {
      case (True, True)   => True
      case (True, Maybe)  => Maybe
      case (True, False)  => False
      case (Maybe, True)  => True
      case (Maybe, Maybe) => Maybe
      case (Maybe, False) => Maybe
      case (False, True)  => True
      case (False, Maybe) => True
      case (False, False) => True
    }
  }
}
