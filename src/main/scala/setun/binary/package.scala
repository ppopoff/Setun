package setun

import scala.language.implicitConversions



package object binary {

  implicit def
  enrichBoolean(bool: Boolean): EnrichedBoolean =  new EnrichedBoolean(bool)

  implicit def
  narrowBoolean(e: EnrichedBoolean): Boolean = e.v


  /** Unary operators, that can't be supported by unary_ */
  def not (bool: EnrichedBoolean) = !bool
  def not (bool: Boolean) = !bool
  def ¬ (bool: EnrichedBoolean) = !bool
  def ¬ (bool: Boolean) = !bool


  private[binary] implicit sealed
  class EnrichedBoolean (val v: Boolean) extends AnyVal {

    private type E = this.type

    //Unary works for + - ! ~
    def unary_~ = new EnrichedBoolean(!v)
    def unary_! = new EnrichedBoolean(!v)


    def and (other: E) = new EnrichedBoolean(other & v)
    def & (other: E) = and(other)
    def ∧ (other: E) = and(other)


    def or (other: E) = new EnrichedBoolean(other | v)
    def + (other: E) = or(other)
    def ∨ (other: E) = or(other)


    def xor (other: E) = new EnrichedBoolean(other ^ v)
    def ⊕ (other: E) = xor(other)
    def ^ (other: E) = xor(other)


    def equiv (p: E) = new EnrichedBoolean((!p || v) && (p || !v))
    def xnor(other: EnrichedBoolean) = equiv(other)
    def ==(other: EnrichedBoolean)   = equiv(other)
    def <=>(other: EnrichedBoolean)  = equiv(other)
    def ≡(other: EnrichedBoolean)    = equiv(other)
    def ⇔(other: EnrichedBoolean)    = equiv(other)


    def implies(other: E) = new EnrichedBoolean((!v) & other)
    def →(other: EnrichedBoolean)  = implies(other)
    def ->(other: EnrichedBoolean) = implies(other)
    def ⊃(other: EnrichedBoolean)  = implies(other)


    /**
      * Sheffer stoke == nand
      */
    def nand(other: E) = new EnrichedBoolean(!(other & v))
    def ↑(other: EnrichedBoolean) = nand(other)
    def |(other: EnrichedBoolean) = nand(other)


    /**
      * Peirce's arrow ↓ nor
      */
    def nor(other: E) = new EnrichedBoolean(!(other | v))
    def ↓(other: EnrichedBoolean) = nor(other)
    def dagger(other: EnrichedBoolean) = nor(other)
  }
}
