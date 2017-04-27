package ohnosequences.fastarious

import SequenceQuality._

/*
  ## SequenceQuality

  A fastq sequence has the sequence itself plus the corresponding quality. At construction, a `SequenceQuality` is checked to have the same length for sequence and quality.
*/
// TODO re-evaluate this design. Probably better to have Seq[(Char, Qual)] as primitive
// NOTE the *constructor* is private here, not the values.
case class SequenceQuality private[fastarious] (val letters: String, val quality: Quality) {

  def isEmpty: Boolean =
    letters.isEmpty

  def length: Int =
    letters.length

  def at(index: Int): Option[(Symbol, Score)] =
    if( index < 0 || (length - 1) < index) None else Some( ( letters(index), quality.scores(index) ) )

  def headOption: Option[(Symbol, Score)] =
    if(isEmpty) None else Some { (letters.head, quality.scores.head) }

  def tailOption: Option[SequenceQuality] =
    if(isEmpty) None else Some { drop(1) }

  def drop(n: Int): SequenceQuality =
    SequenceQuality( letters drop n, Quality( quality.scores drop n ) )

  def dropRight(n: Int): SequenceQuality =
    SequenceQuality( letters dropRight n, Quality( quality.scores dropRight n ) )

  def slice(from: Int, until: Int): SequenceQuality =
    SequenceQuality( letters.slice(from, until), Quality( quality.scores.slice(from, until) ) )

  def take(n: Int): SequenceQuality =
    SequenceQuality( letters take n, Quality( quality.scores take n ) )

  def takeRight(n: Int): SequenceQuality =
    SequenceQuality( letters takeRight n, Quality( quality.scores takeRight n ) )

  def ++(other: SequenceQuality): SequenceQuality =
    SequenceQuality(letters ++ other.letters, Quality( quality.scores ++ other.quality.scores ))

  def asStringPhred33: String = Seq(
    letters,
    "+",
    quality.toPhred33
  ).mkString("\n")

  /*
    ### Filtering operations

    All these methods (filter, takeWhile/dropWhile, etc) take a predicate on *both* letters and quality. There are convenience methods `xyzSequence` and `xyzQuality` which have the same behavior as `xyz` but for predicates on letters and quality respectively.
  */

  /*
    #### filter
  */
  def filter(p: (Symbol, Score) => Boolean): SequenceQuality = {
    val (seq, qual) =
      (letters zip quality.scores)
        .filter { case (c, q) => p(c, q) }
        .unzip

    SequenceQuality(seq.mkString, Quality(qual))
  }

  def filterSequence(p: Symbol => Boolean): SequenceQuality =
    filter { (s, _) => p(s) }

  def filterQuality(p: Score => Boolean): SequenceQuality =
    filter { (_, q) => p(q) }

  /*
    #### count
  */
  def count(p: (Symbol, Score) => Boolean): Int =
    (letters zip quality.scores)
      .count { case (c, q) => p(c, q) }

  def countSequence(p: Symbol => Boolean): Int =
    count { (s, _) => p(s) }

  def countQuality(p: Score => Boolean): Int =
    count { (_, q) => p(q) }

  /*
    #### takeWhile
  */
  def takeWhile(p: (Symbol, Score) => Boolean): SequenceQuality = {
    val (seq, qual) =
      (letters zip quality.scores)
        .takeWhile { case (c, q) => p(c, q) }
        .unzip

    SequenceQuality(seq.mkString, Quality(qual))
  }

  def takeWhileQuality(p: Score => Boolean): SequenceQuality =
    takeWhile { (_, q) => p(q) }

  def takeWhileSequence(p: Symbol => Boolean): SequenceQuality =
    takeWhile { (s, _) => p(s) }

  /*
    #### dropWhile
  */
  def dropWhile(p: (Symbol, Score) => Boolean): SequenceQuality = {
    val (seq, qual) =
      (letters zip quality.scores)
        .dropWhile { case (c, q) => p(c, q) }
        .unzip

    SequenceQuality(seq.mkString, Quality(qual))
  }

  def dropWhileQuality(p: Score => Boolean): SequenceQuality =
    dropWhile { (_, q) => p(q) }

  def dropWhileSequence(p: Symbol => Boolean): SequenceQuality =
    dropWhile { (s, _) => p(s) }

  /*
    #### span
  */
  def span(p: (Symbol, Score) => Boolean): (SequenceQuality, SequenceQuality) = {
    val (sq1, sq2) =
      (letters zip quality.scores)
        .span { case (c, q) => p(c, q) }

    val (s1, q1) = sq1.unzip
    val (s2, q2) = sq2.unzip

    (
      SequenceQuality(s1.mkString, Quality(q1)),
      SequenceQuality(s2.mkString, Quality(q2))
    )
  }

  def spanQuality(p: Score => Boolean): (SequenceQuality, SequenceQuality) =
    span { (_, q) => p(q) }

  def spanSequence(p: Symbol => Boolean): (SequenceQuality, SequenceQuality) =
    span { (s, _) => p(s) }
}

case object SequenceQuality {

  type Symbol = Char
  type Score  = Int

  def fromStringsPhred33(rawSeq: String, rawQual: String): Option[SequenceQuality] =
    if(rawSeq.length == rawQual.length)
      Quality.fromPhred33(rawQual).map( SequenceQuality(rawSeq, _) )
    else
      None
}
