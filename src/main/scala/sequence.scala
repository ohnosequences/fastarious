package ohnosequences.fastarious

/*
  ## Sequence
*/
final class Sequence(val letters: Array[Char]) {

  override def equals(that: Any): Boolean =
    that match {
      case that: Sequence => java.util.Arrays.equals(letters, that.letters)
      case _              => false
   }

  def isEmpty: Boolean =
    letters.isEmpty

  def length: Int =
    letters.length

  def at(index: Int): Option[Symbol] =
    if( index < 0 || (length - 1) < index) None else Some( letters(index) )

  def headOption: Option[Symbol] =
    if(isEmpty) None else Some { letters.head }

  def tailOption: Option[Sequence] =
    if(isEmpty) None else Some { drop(1) }

  def drop(n: Int): Sequence =
    Sequence( letters drop n )

  def dropRight(n: Int): Sequence =
    Sequence( letters dropRight n )

  def slice(from: Int, until: Int): Sequence =
    Sequence( letters.slice(from, until) )

  def take(n: Int): Sequence =
    Sequence( letters take n )

  def takeRight(n: Int): Sequence =
    Sequence( letters takeRight n )

  def :+(s: Symbol): Sequence =
    Sequence(letters :+ s)

  def +:(s: Symbol): Sequence =
    Sequence(s +: letters)

  def ++(other: Sequence): Sequence =
    Sequence(letters ++ other.letters )

  def foldLeft[X](init: X)(op: (X,Symbol) => X): X =
    letters.foldLeft(init)(op)

  def reverse: Sequence =
    Sequence(letters.reverse)
}

case object Sequence {

  def apply(arr: Array[Char]): Sequence =
    new Sequence(arr)

  def apply(str: String): Sequence =
    new Sequence(str.toCharArray)

  val empty: Sequence =
    Sequence(Array[Char]())
}
