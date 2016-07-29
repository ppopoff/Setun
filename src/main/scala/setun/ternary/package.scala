package setun

/**
  * This package contains Ternary class that
  * contains Ternary logic and supported
  * operations
  */
package object logical {

  sealed trait Logical
  case object True  extends Logical
  case object False extends Logical
  case object Maybe extends Logical

  def unary_!(t: Logical) = not(t)
  def ¬ (t: Logical) = not(t)
  def ait(t: Logical) = not(t)
  def negate(t: Logical) = not(t)

  def not (t: Logical) = t match {
    case True  => False
    case Maybe => Maybe
    case False => True
  }


  implicit class TernaryOps(t: Logical) {
    def && (other: Logical) = t and other
    def || (other: Logical) = t or other

    def ∧ (other: Logical) = t and other
    def ∨ (other: Logical) = t or other
    def ≡ (other: Logical) = t equiv other
    def ⊃ (other: Logical) = t imply other

    def and (other: Logical) = (t, other) match {
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

    def or (other: Logical) = (t, other) match {
      case (True, True)   => True
      case (True, Maybe)  => True
      case (True, False)  => True
      case (Maybe, True)  => True
      case (Maybe, Maybe) => Maybe
      case (Maybe, False) => Maybe
      case (False, True)  => True
      case (False, Maybe) => Maybe
      case (False, False) => False
    }

    def equiv(other: Logical) = (t, other) match {
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

    def imply(other: Logical) = (t, other) match {
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
