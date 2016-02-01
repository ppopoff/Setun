package setun.ternary

import org.junit.runner.RunWith
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TernaryOperationsSpec extends FunSpec with Matchers {
  describe("Here's the test") {
    import setun.ternary._

    describe("and operator") {
      it ("should be associative") {
        True  and True should  be (True  and True)
        True  and False should be (False and True)
        Maybe and Maybe should be (Maybe and Maybe)
        True  and Maybe should be (Maybe and True)
        False and Maybe should be (Maybe and False)
      }
    }


    describe("or operator") {
      it ("should be associative") {
        True  or True  should be (True  or True)
        True  or False should be (False or True)
        Maybe or Maybe should be (Maybe or Maybe)
        True  or Maybe should be (Maybe or True)
        False or Maybe should be (Maybe or False)
      }
    }
  }
}
