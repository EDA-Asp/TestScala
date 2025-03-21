package Katas

import org.scalatest.funsuite.AnyFunSuite
import playgraund.Katas.{booleanToStringInterpolated, booleanToStringToString}

class BoolToStrTest extends AnyFunSuite:
  test("true is true") {

    assert("true" === booleanToStringToString(true))
    assert("true" === booleanToStringInterpolated(true))

  }

  test("false is false") {

    assert("false" === booleanToStringToString(false))
    assert("false" === booleanToStringInterpolated(false))

  }
