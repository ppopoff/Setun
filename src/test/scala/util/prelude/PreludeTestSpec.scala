package util.prelude

import org.junit.runner.RunWith
import org.scalatest.{Matchers, FunSpec}
import org.scalacheck.Prop.forAll
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PreludeTestSpec extends FunSpec with Matchers {
  describe("Function extensions") {

    describe("Function composition") {
      it ("should behave the same as scala compose method of F1") {
        forAll {(f1: Int => Int, f2: Int => Int, value: Int) =>
          (f1 o f2)(value) == (f1 compose f2)(value)
        }.check

        forAll {(f1: String => String, f2: String => String, value: String) =>
          (f1 o f2)(value) == (f1 compose f2)(value)
        }.check
      }
    }
  }
}
