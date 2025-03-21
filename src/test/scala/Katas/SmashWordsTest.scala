package Katas

import org.scalatest.funsuite.AnyFunSuite
import playgraund.Katas.{smash_mkString, smash_reduce, smash_reduceOption}

class SmashWordsTest extends AnyFunSuite:
  test("one string as is") {

    assert(
      List(smash_mkString, smash_reduce, smash_reduceOption).map(f =>
        f(List("aa"))
      ) === List.fill(3)("aa")
    )

  }

  test("several strings as sentence") {

    assert(smash_mkString(List("aa", "bb", "", "")) === "aa bb  ")
    assert(smash_reduce(List("aa", "bb", "", "")) === "aa bb  ")

  }

  test("empty list as empty string") {

    assert(smash_mkString(List()) === "")

  }
