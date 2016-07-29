package setun

/**
  * This package contains Ternary class that
  * contains Ternary logic and supported
  * operations
  */
package object logical {

  sealed trait Logical extends AnyVal
  case object True  extends AnyVal with Logical { val value: Byte =  1 }
  case object False extends AnyVal with Logical { val value: Byte = -1 }
  case object Maybe extends AnyVal with Logical { val value: Byte =  0 }

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
      case (Maybe, False) => False
      case (Maybe, _)     => Maybe
      case (False, _)     => False
    }

    def or (other: Logical) = (t, other) match {
      case (True, _)      => True
      case (Maybe, True)  => True
      case (Maybe, _)     => Maybe
      case (False, True)  => True
      case (False, Maybe) => Maybe
      case (False, False) => False
    }

    def equiv(other: Logical) = (t, other) match {
      case (True, True)   => True
      case (True, Maybe)  => Maybe
      case (True, False)  => False
      case (Maybe, _)     => Maybe
      case (False, True)  => False
      case (False, Maybe) => Maybe
      case (False, False) => True
    }

    def imply(other: Logical) = (t, other) match {
      case (True, True)   => True
      case (True, Maybe)  => Maybe
      case (True, False)  => False
      case (Maybe, True)  => True
      case (Maybe, _)     => Maybe
      case (False, _)     => True
    }
  }
}
