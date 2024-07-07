import scala.collection.mutable.ArrayBuffer
import scala.annotation.targetName
@main def forComprehensions = {
    usingSequence
    usingSequence2
}

def usingSequence = {
  val seq1 = Sequence(1, 2, 3, 4, 5)
  val seq2 = Sequence("Ana", "Ricardo", "Beatriz")

  for (value <- seq1) println(value)
  for (value <- seq2) println(value)

  seq1.foreach(println)
  seq2.foreach(println)
}

def usingSequence2 = {
  val seq1 = Sequence2(1, 2, 3, 4, 5)
  val seq2 = Sequence2("Ana", "Ricardo", "Beatriz")

  for (value <- seq1) println(value)
  for (value <- seq2) println(value)

  seq1.foreach(println)
  seq2.foreach(println)
}

class Sequence[A](values: A*) {
  private val _values = ArrayBuffer[A]()

  _values ++= values

  def foreach(f: A => Unit): Unit = _values.foreach(f)
}

object Sequence {
  def apply[A](values: A*) = new Sequence(values)
}

case class Sequence2[A](initialElems: A*) {
  private val elems = ArrayBuffer[A]()

  elems ++= initialElems

  def foreach(b: A => Unit): Unit = elems.foreach(b)
}

abstract class CustomClass[A] {
  def map[B](f: A => B): CustomClass[B]
  def flatMap[B](f: A => CustomClass[B]): CustomClass[B]
  def withFilter(p: A => Boolean): CustomClass[A]
  def foreach(b: A => Unit): Unit
}
