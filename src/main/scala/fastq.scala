package ohnosequences.fastarious

import fasta._
import ohnosequences.cosas._, types._, records._, fns._, klists._
import java.io._
import spire.algebra._
import spire.math._
import spire.implicits._

case object fastq {

  /*
    ## Sequence

    A fastq sequence has the sequence itself plus the corresponding quality. At construction, a `Sequence` is checked to have the same length for sequence and quality.
  */
  // TODO re-evaluate this design. Probably better to have Seq[(Char, Qual)] as primitive
  // NOTE the *constructor* is private here, not the values.
  case class Sequence private[fastarious] (val sequence: String, val quality: Quality) {

    def isEmpty =
      sequence.isEmpty

    def length =
      sequence.length

    def at(index: Int): Option[(Char, Int)] =
      if( index < 0 || (length - 1) < index) None else Some( ( sequence(index), quality.scores(index) ) )

    def headOption: Option[(Char, Int)] =
      if(isEmpty) None else Some { (sequence.head, quality.scores.head) }

    def tailOption: Option[Sequence] =
      if(isEmpty) None else Some { drop(1) }

    def drop(n: Int): Sequence =
      Sequence( sequence drop n, Quality( quality.scores drop n ) )

    def dropRight(n: Int): Sequence =
      Sequence( sequence dropRight n, Quality( quality.scores dropRight n ) )

    def slice(from: Int, until: Int): Sequence =
      Sequence( sequence.slice(from, until), Quality( quality.scores.slice(from, until) ) )

    def take(n: Int): Sequence =
      Sequence( sequence take n, Quality( quality.scores take n ) )

    def takeRight(n: Int): Sequence =
      Sequence( sequence takeRight n, Quality( quality.scores takeRight n ) )

    def ++(other: Sequence): Sequence =
      Sequence(sequence ++ other.sequence, Quality( quality.scores ++ other.quality.scores ))

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
        (sequence zip quality.scores)
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
      (sequence zip quality.scores)
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
        (sequence zip quality.scores)
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
        (sequence zip quality.scores)
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
        (sequence zip quality.scores)
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

  /*
    ## Quality

    [Phred Quality scores](https://en.wikipedia.org/wiki/Phred_quality_score), thus assumed to be positive.
  */
  case class Quality(val scores: Seq[Int]) extends AnyVal {

    import Quality._

    def toPhred33: String =
      (scores map Quality.toPhred33).mkString

    // see for example https://doi.org/10.1093/bioinformatics/btv401
    def expectedErrors: BigDecimal =
      scores.foldLeft(0:BigDecimal)(
        { (acc,s) => acc + s.asPhredScore.errorProbability }
      )

    def variance: BigDecimal =
      scores.foldLeft(0:BigDecimal)(
        { (acc,s) => acc + ( s.asPhredScore.errorProbability * s.asPhredScore.successProbability ) }
      )

    def maxScore: Int =
      scores.max

    def minScore: Int =
      scores.min
  }

  case object Quality {

    private def toProb(n: Int): BigDecimal =
      BigDecimal(10) fpow ( - BigDecimal(n / 10) )

    private def cacheProbs: Map[Int, BigDecimal] =
      Map( (0 to 100).map { n => (n, toProb(n)) } : _*)

    lazy val phred33Cache: Map[Int, BigDecimal] =
      cacheProbs

    implicit class ScoreConverters(val n: Int) extends AnyVal {

      def asPhredScore: PhredScore =
        PhredScore(n)
    }

    case class PhredScore(val n: Int) extends AnyVal {

      def errorProbability: BigDecimal =
        phred33Cache.getOrElse(n, toProb(n))

      def successProbability: BigDecimal =
        1 - errorProbability
    }

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

    val toPhred33: Int => Char =
      { i => (i + 33).toChar }
  }

  /*
    Here `value` is assumed to correspond to the real id, *without* the `@` character which would identify it in its serialized form. So, do *not* pass `"@HWI:2321"`, write instead `Id("HWI:2321")`
  */
  case class Id private[fastarious] (val value: String) extends AnyVal {

    def asString: String =
      s"@${value.filterNot(_ == '\n')}"

    def isEmpty: Boolean =
      value.isEmpty
  }

  case object Id {

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

  case class FASTQ(val id: Id, val value: Sequence) {

    def asStringPhred33: String = Seq(
      id.asString,
      value.asStringPhred33
    ).mkString("\n")

    def toFASTA: FASTA.Value = FASTA(
      fasta.header( FastaHeader(id.value) )              ::
      fasta.sequence( FastaSequence(value.sequence) ) ::
      *[AnyDenotation]
    )

    def updateSequence(f: Sequence => Sequence): FASTQ =
      this.copy(value = f(this.value))
  }

  case object FASTQ {

    def fromStringsPhred33(
      id        : String,
      sequence  : String,
      quality   : String
    )
    : Option[FASTQ] =
      Sequence.fromStringsPhred33(sequence, quality) map { FASTQ( Id(id), _ ) }
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
        .grouped(4).filter(_.size == 4) // grouped is broken
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
