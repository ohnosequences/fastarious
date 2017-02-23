package ohnosequences.fastarious

import fasta._
import ohnosequences.cosas._, types._, records._, fns._, klists._
import java.io._

case object fastq {

  /*
    ## Sequence

    A fastq sequence has the sequence itself plus the corresponding quality. At construction, a `Sequence` is checked to have the same length for sequence and quality.
  */
  // TODO re-evaluate this design. Probably better to have Seq[(Char, Qual)] as primitive
  case class Sequence private[fastarious] (val sequence: String, val quality: Quality) {

    def isEmpty =
      sequence.isEmpty

    def length =
      sequence.length

    def at(index: Int): Option[(Char, Int)] =
      if(0 <= index && index <= length - 1)
        Some( ( sequence(index), quality.value(index) ) )
      else
        None

    def drop(n: Int): Sequence =
      Sequence( sequence drop n, Quality( quality.value drop n ) )

    def dropRight(n: Int): Sequence =
      Sequence( sequence dropRight n, Quality( quality.value dropRight n ) )

    def slice(from: Int, until: Int): Sequence =
      Sequence( sequence.slice(from, until), Quality( quality.value.slice(from, until) ) )

    def take(n: Int): Sequence =
      Sequence( sequence take n, Quality( quality.value take n ) )

    def takeRight(n: Int): Sequence =
      Sequence( sequence takeRight n, Quality( quality.value takeRight n ) )

    def ++(other: Sequence): Sequence =
      Sequence(sequence ++ other.sequence, Quality( quality.value ++ other.quality.value ))

    def asStringPhred33: String = Seq(
      sequence,
      "+",
      quality.toPhred33
    ).mkString("\n")

    /*
      ### Filtering operations

      All these methods (filter, takeWhile/dropWhile, etc) take a predicate on *both* sequence and quality. There are convenience methods `xyzSequence` and `xyzQuality` which have the same behavior as `xyz` but for predicates on sequence and quality respectively.
    */

    /*
      #### filter
    */
    def filter(p: (Char, Int) => Boolean): Sequence = {
      val (seq, qual) =
        (sequence zip quality.value)
          .filter { case (c, q) => p(c, q) }
          .unzip

      Sequence(seq.mkString, Quality(qual))
    }

    def filterSequence(p: Char => Boolean): Sequence =
      filter { (s, _) => p(s) }

    def filterQuality(p: Int => Boolean): Sequence =
      filter { (_, q) => p(q) }

    /*
      #### count
    */
    def count(p: (Char, Int) => Boolean): Int =
      (sequence zip quality.value)
        .count { case (c, q) => p(c, q) }

    def countSequence(p: Char => Boolean): Int =
      count { (s, _) => p(s) }

    def countQuality(p: Int => Boolean): Int =
      count { (_, q) => p(q) }

    /*
      #### takeWhile
    */
    def takeWhile(p: (Char, Int) => Boolean): Sequence = {
      val (seq, qual) =
        (sequence zip quality.value)
          .takeWhile { case (c, q) => p(c, q) }
          .unzip

      Sequence(seq.mkString, Quality(qual))
    }

    def takeWhileQuality(p: Int => Boolean): Sequence =
      takeWhile { (_, q) => p(q) }

    def takeWhileSequence(p: Char => Boolean): Sequence =
      takeWhile { (s, _) => p(s) }

    /*
      #### dropWhile
    */
    def dropWhile(p: (Char, Int) => Boolean): Sequence = {
      val (seq, qual) =
        (sequence zip quality.value)
          .dropWhile { case (c, q) => p(c, q) }
          .unzip

      Sequence(seq.mkString, Quality(qual))
    }

    def dropWhileQuality(p: Int => Boolean): Sequence =
      dropWhile { (_, q) => p(q) }

    def dropWhileSequence(p: Char => Boolean): Sequence =
      dropWhile { (s, _) => p(s) }

    /*
      #### span
    */
    def span(p: (Char, Int) => Boolean): (Sequence, Sequence) = {
      val (sq1, sq2) =
        (sequence zip quality.value)
          .span { case (c, q) => p(c, q) }

      val (s1, q1) = sq1.unzip
      val (s2, q2) = sq2.unzip

      (
        Sequence(s1.mkString, Quality(q1)),
        Sequence(s2.mkString, Quality(q2))
      )
    }

    def spanQuality(p: Int => Boolean): (Sequence, Sequence) =
      span { (_, q) => p(q) }

    def spanSequence(p: Char => Boolean): (Sequence, Sequence) =
      span { (s, _) => p(s) }
  }

  case object Sequence {

    def fromStringsPhred33(rawSeq: String, rawQual: String): Option[Sequence] =
      if(rawSeq.length == rawQual.length)
        Quality.fromPhred33(rawQual).map( Sequence(rawSeq, _) )
      else
        None
  }

  case class  Quality private[fastarious] (val value: Seq[Int]) extends AnyVal {

    def toPhred33: String =
      (value map Quality.toPhred33).mkString

    def average: BigDecimal =
      if(value.isEmpty)
        0
      else
        value.foldLeft(0: BigDecimal){ (acc, v) => acc + (v: BigDecimal) } / (value.length)
  }

  case object Quality {

    def fromPhred33(raw: String): Option[Quality] = {

      @annotation.tailrec
      def rec(cs: String, acc: collection.mutable.Builder[Int,Vector[Int]], errors: Boolean): Option[Quality] =
        if(errors) { None } else {

          if(cs.isEmpty) Some( Quality(acc.result) ) else {

            cs.head.toInt match {
              case q if 33 <= q && q <= 126 => rec(cs.tail, acc += (q - 33), errors)
              case _                        => rec(cs, acc, errors = true)
            }
          }
        }

      val bldr = Vector.newBuilder[Int]; bldr.sizeHint(raw.length)
      
      rec(raw, bldr, false)
    }

    val toPhred33: Int => Char = { i => (i + 33).toChar }
  }

  case class Id private[fastarious] (val value: String) extends AnyVal {

    def asString: String =
      s"@${value}"

    def isEmpty: Boolean =
      value.isEmpty
  }

  case object Id {

    def from(raw: String): Id =
      new Id(raw.filterNot(_ == '\n'))

    /*
      ### Id parsing

      These methods are *not* for building Id values; for that use `from`.
    */
    private val isValid: String => Boolean =
      _ startsWith "@"

    /*
      `parseFrom` will drop the *first* '@' in `raw`, if any. My understanding is that `@@hola` is a valid (albeit confusing) FASTQ id.
    */
    def parseFrom(raw: String): Option[Id] =
      if(isValid(raw)) Some( new Id(raw.stripPrefix("@")) ) else None
  }

  case class FASTQ(val id: Id, val sequence: Sequence) {

    def asStringPhred33: String = Seq(
      id.asString,
      sequence.asStringPhred33
    ).mkString("\n")

    def toFASTA: FASTA.Value = FASTA(
      fasta.header( FastaHeader(id.value) )              ::
      fasta.sequence( FastaSequence(sequence.sequence) ) ::
      *[AnyDenotation]
    )
  }

  case object FASTQ {

    def fromStringsPhred33(
      id        : String,
      sequence  : String,
      quality   : String
    )
    : Option[FASTQ] =
      Sequence.fromStringsPhred33(sequence, quality) map { FASTQ( Id.from(id), _ ) }
  }

  implicit class FASTQIteratorOps(val fastqs: Iterator[FASTQ]) extends AnyVal {

    def appendAsPhred33To(file: File): File = {

      if(fastqs.hasNext) {
        val wr = new BufferedWriter(new FileWriter(file, true))

        fastqs.foreach { fq =>
          wr.write( fq.asStringPhred33 )
          wr.newLine
        }
        wr.close
      }

      file
    }
  }

  implicit class IteratorFASTQOps(val lines: Iterator[String]) extends AnyVal {

    def parseFastqPhred33: Iterator[Option[FASTQ]] =
      lines
        .grouped(4)
        .map { quartet =>

          if( quartet(2) startsWith "+" ) {
            for {
              i <- Id.parseFrom( quartet(0) )
              s <- Sequence.fromStringsPhred33( quartet(1), quartet(3) )
            } yield FASTQ(i, s)
          }
          else None
        }

    def parseFastqPhred33DropErrors: Iterator[FASTQ] =
      parseFastqPhred33 collect { case Some(fq) => fq }
  }
}
