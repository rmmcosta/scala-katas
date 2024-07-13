import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

@main def forComprehensions1 = {
  val seq1 = MySequence(1, 2, 3, 4, 5)

  // using for loop to print the elements
  println("--- all elements ---")
  for (i <- seq1) println(i)

  // using for expression/comprehension to print the even numbers
  val evens = for {
    i <- seq1
    if (i % 2 == 0)
  } yield (i)

  println("--- all even elements ---")
  for (i <- evens) println(i)

  // using for comprehension to print all uppercase version of a given seq of names
  val uppers = for {
    name <- MySequence("Ricardo", "Ana", "Beatriz")
  } yield (name.toUpperCase)

  println("--- all uppers ---")
  for (upper <- uppers) println(upper)

  val myFriends = MySequence("Amadeu", "Nelson", "Ramos", "Branquinho")
  val anaFriends = MySequence("Miguel", "Angela", "Ramos", "Branquinho")

  val commonFriends = for {
    myFriend <- myFriends
    anaFriend <- anaFriends
    if (myFriend == anaFriend)
  } yield myFriend

  println("--- common friends ---")
  for (commonFriend <- commonFriends) println(commonFriend)
}

private case class MySequence[A](initialValues: A*) {

  private def isEmpty = initialValues.isEmpty

  def foreach(b: A => Unit): Unit = {
    @tailrec
    def foreachIter(elements: Seq[A]): Unit = {
      if (elements.nonEmpty) {
        b(elements.head)
        foreachIter(elements.tail)
      }
    }

    foreachIter(initialValues)
  }

  def withFilter(f: A => Boolean): MySequence[A] = {
    initialValues.foldLeft(MySequence[A]())((acc: MySequence[A], value: A) =>
      if (f(value)) {
        val updatedValues = acc.initialValues :+ value
        MySequence(updatedValues*)
      } else acc
    )
  }

  def map[B](f: A => B): MySequence[B] = {
    @tailrec
    def mapIter(
        currentMapped: MySequence[B],
        currentUnmapped: MySequence[A]
    ): MySequence[B] = {
      if (currentUnmapped.isEmpty) currentMapped
      else {
        val updatedMapped =
          currentMapped.initialValues :+ f(currentUnmapped.initialValues.head)
        val updatedUnmapped = currentUnmapped.initialValues.tail
        mapIter(MySequence(updatedMapped*), MySequence(updatedUnmapped*))
      }
    }

    mapIter(MySequence[B](), this)
  }

  def flatMap[B](f: A => MySequence[B]): MySequence[B] = flatten(map(f))

  private def flatten[B](listValues: MySequence[MySequence[B]]): MySequence[B] = {
    @tailrec
    def flattenIter(
        listValues: MySequence[MySequence[B]],
        alreadyFlatten: MySequence[B]
    ): MySequence[B] = {
      if (listValues.isEmpty) alreadyFlatten
      else {
        val flattenLeafs = flatten(listValues.initialValues.head, Seq[B]())
        flattenIter(
          MySequence(listValues.initialValues.tail*),
          MySequence(alreadyFlatten.initialValues ++ flattenLeafs*)
        )
      }
    }

    flattenIter(listValues, MySequence[B]())
  }

  @tailrec
  private def flatten[B](
      listValues: MySequence[B],
      alreadyFlatten: Seq[B]
  ): Seq[B] = {
    if (listValues.isEmpty) alreadyFlatten
    else {
      val updatedFlatten = alreadyFlatten :+ listValues.initialValues.head
      flatten(MySequence(listValues.initialValues.tail*), updatedFlatten)
    }
  }
}
