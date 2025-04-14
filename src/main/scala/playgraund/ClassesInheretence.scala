package playgraund

import playgraund.Element.elem

object ClassesInheretence extends App {

  val a = elem("Line")
//  val b = elem(Vector("V1l1","V1l2"))
//  val c = elem('#',3,1)
//  println(a besides b besides c)

}

// https://stackoverflow.com/questions/12184997/scala-and-forward-references

abstract class Element:
  def height: Int = contents.length

  def contents: Vector[String]

  def width: Int = if height == 0 then 0 else contents(0).length

  def above(that: Element): Element =
    elem(this.contents ++ that.contents)

  def besides(that: Element): Element =
    elem(for (a, b) <- this.contents zip that.contents yield a + b)

  override def toString: String = contents.mkString("\n")

object Element:
  def elem(contents: Vector[String]): Element = VectorElement(contents)

  def elem(line: String): Element = LineElement(line)

  def elem(ch: Char, width: Int, height: Int): Element = UniformElement(ch, width, height)

//he name conts of the parameter was chosen just so that
//it would look similar to the field name contents without actually clashing
//with it.This is a  “code smell, ”a sign that there may be some unnecessary redundancy and repetition in your code

//class VectorElement(cont: Vector[String]) extends Element:
//  val contents: Vector[String] = cont

//Note that now the contents parameter is prefixed by val. This is a short-
//hand that defines at the same time a parameter and field with the same
//name. Specifically, class VectorElement now has an (unreassignable) field
//contents, which can be accessed from outside the class. The field is initial-
//ized with the value of the parameter

private class VectorElement(val contents: Vector[String]) extends Element

//In fact, we defined LineElement as a subclass of VectorElement primarily
//to reuse VectorElement’s definition of contents. Perhaps it would
//be better, therefore, to define LineElement as a direct subclass of Element
//class LineElement(s: String) extends VectorElement(Vector(s)):
//  override def width: Int = s.length
//
//  override val height: Int = 1

private class LineElement(s: String) extends Element:

  override val width: Int = s.length
  override val height: Int = 1
  val contents: Vector[String] = Vector(s)

private class UniformElement(
    ch: Char,
    override val width: Int,
    override val height: Int
) extends Element:

  private val line = ch.toString * width

  val contents: Vector[String] = Vector.fill(height)(line)

class Cat:
  val dangerous = false

//  class Tiger(param1: Boolean, param2: Int) extends Cat:
//
//  override val dangerous = param1
//  private var age = param2

class Tiger(override val dangerous: Boolean, private var age: Int) extends Cat
