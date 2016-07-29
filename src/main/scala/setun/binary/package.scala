package setun

import scala.language.implicitConversions

/**
  * The package contains Binary class that can be
  * seen as enriched version of Boolean class
  */
package object binary {

  implicit def
  enrichBoolean(bool: Boolean): Binary =  new Binary(bool)

  implicit def
  narrowBoolean(e: Binary): Boolean = e.v


  /** Unary operators, that can't be supported by unary_ */
  def not (bool: Boolean) = !bool
  def aint (bool: Boolean) = !bool
  def ¬ (bool: Boolean) = !bool


  private[binary] implicit
  class Binary (val v: Boolean) extends AnyVal {

    //Unary works for + - ! ~
    def unary_~ = new Binary(!v)
    def unary_! = new Binary(!v)

    def and (other: Binary) = new Binary(other && v)
    def & (other: Binary) = and(other)
    def ∧ (other: Binary) = and(other)

    def or (other: Binary) = new Binary(other || v)
    def + (other: Binary) = or(other)
    def ∨ (other: Binary) = or(other)

    def xor (other: Binary) = new Binary(other ^ v)
    def ⊕ (other: Binary) = xor(other)

    def equiv (p: Binary) = new Binary((!p || v) && (p || !v))
    def xnor(other: Binary) = equiv(other)
    def ==(other: Binary)   = equiv(other)
    def <=>(other: Binary)  = equiv(other)
    def ≡(other: Binary)    = equiv(other)
    def ⇔(other: Binary)    = equiv(other)

    def implies(other: Binary) = new Binary((!v) & other)
    def →(other: Binary)  = implies(other)
    def ->(other: Binary) = implies(other)
    def ⊃(other: Binary)  = implies(other)


    /**
      * Sheffer's stoke == nand
      */
    def nand(other: Binary) = new Binary(!(narrowBoolean(other) & v))
    def ↑(other: Binary) = nand(other)
    def |(other: Binary) = nand(other)

    /**
      * Peirce's arrow ↓ nor
      */
    def nor(other: Binary) = new Binary(!(other || v))
    def ↓(other: Binary) = nor(other)
    def dagger(other: Binary) = nor(other)
  }
}
