package playgraund

import playgraund.Element.elem

object ClassesInheretence extends App {

  val a = elem("Linewwwwww")
  val b = elem(Vector("V1l1", "V1l2"))
  val c = elem('a', 5, 3)

  println((a beside b))

  println(besideMy(a, b))

}

// https://stackoverflow.com/questions/12184997/scala-and-forward-references

abstract class Element:
  def contents: Vector[String]
  def width: Int = if height == 0 then 0 else contents(0).length
  def height: Int = contents.length
  infix def above(that: Element): Element =
    val this1 = this.widen(that.width)
    val that1 = that.widen(this.width)
    elem(this1.contents ++ that1.contents)
  infix def beside(that: Element): Element =
    val this1 = this.heighten(that.height)
    val that1 = that.heighten(this.height)
    elem(
      for (line1, line2) <- this1.contents.zip(that1.contents)
      yield line1 + line2
    )
  def widen(w: Int): Element =
    if w <= width then this
    else
      val left = elem(' ', (w - width) / 2, height)
      val right = elem(' ', w - width - left.width, height)
      left beside this beside right
  def heighten(h: Int): Element =
    if h <= height then this
    else
      val top = elem(' ', width, (h - height) / 2)
      val bot = elem(' ', width, h - height - top.height)
      top above this above bot
  override def toString = contents.mkString("\n")
end Element

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

  val line = ch.toString * width

  val contents: Vector[String] = Vector.fill(height)(line)

class Cat:
  val dangerous = false

//  class Tiger(param1: Boolean, param2: Int) extends Cat:
//
//  override val dangerous = param1
//  private var age = param2

class Tiger(override val dangerous: Boolean, private var age: Int) extends Cat

def heighten(element: Element, h: Int): Element =
  if element.height >= h then element
  else
    val top = elem(' ', element.width, (h - element.height) / 2)
    val bot = elem(' ', element.width, h - element.height - top.height)
    aboveMy(aboveMy(top, element), bot)

def widen(element: Element, w: Int): Element =
  if element.width >= w then element
  else
    val left = elem(' ', (w - element.width) / 2, element.height)
    val right = elem(' ', w - element.width - left.width, element.height)
    besideMy(besideMy(left, element), right)

def aboveMy(e1: Element, e2: Element): Element =
  val e1w = widen(e1, e2.width)
  val e2w = widen(e2, e1.width)
  elem(e1w.contents ++ e2w.contents)

def besideMy(e1: Element, e2: Element): Element =

  val e1h = heighten(e1, e2.height)
  val e2h = heighten(e2, e1.height)

  elem(
    for (line1, line2) <- e1h.contents zip e2h.contents
    yield line1 + line2
  )
