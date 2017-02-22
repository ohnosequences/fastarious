package ohnosequences.fastarious

import fasta._
import ohnosequences.cosas._, types._, records._, fns._, klists._
import java.io._

case object fastq {

  /*

    ## Sequence

    A fastq sequence has the sequence itself plus the corresponding quality. They are checked at construction.
  */
  case class Sequence private[fastarious] (val sequence: String, val quality: Quality) {

    def length =
      sequence.length

    def drop(n: Int): Sequence =
      Sequence( sequence drop n, Quality( quality.value drop n ) )

    def dropRight(n: Int): Sequence =
      Sequence( sequence dropRight n, Quality( quality.value dropRight n ) )

    def slice(from: Int, until: Int): Sequence =
      Sequence( sequence.slice(from, until), Quality( quality.value.slice(from, until) ) )

    def ++(other: Sequence): Sequence =
      Sequence(sequence ++ other.sequence, Quality( quality.value ++ other.quality.value ))

    def asStringPhred33: String =
      s"${sequence}\n+\n${quality.toPhred33}"

    def filter(p: (Char, Int) => Boolean): Sequence = {

      val (seq, qual) =
        (sequence zip quality.value)
          .filter({ cq => p(cq._1, cq._2) })
          .unzip

      Sequence(seq.mkString, Quality(qual))
    }

    def filterSequence(p: Char => Boolean): Sequence =
      filter({ (s,q) => p(s) })

    def filterQuality(p: Int => Boolean): Sequence =
      filter({ (s,q) => p(q) })

    def takeWhile(p: (Char, Int) => Boolean): Sequence = {
      val (seq, qual) =
        (sequence zip quality.value)
          .takeWhile({ cq => p(cq._1, cq._2) })
          .unzip

      Sequence(seq.mkString, Quality(qual))
    }

    def dropWhile(p: (Char, Int) => Boolean): Sequence = {
      val (seq, qual) =
        (sequence zip quality.value)
          .dropWhile({ cq => p(cq._1, cq._2) })
          .unzip

      Sequence(seq.mkString, Quality(qual))
    }

    def dropWhileQuality(p: Int => Boolean): Sequence =
      dropWhile({ (s,q) => p(q) })
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
      if(value.isEmpty) 0 else {

        val sum: BigDecimal = value.foldLeft(0: BigDecimal){ (acc, v) => acc + (v:BigDecimal) }

        sum / (value.length)
      }
  }

  case object Quality {

    def fromPhred33(raw: String): Option[Quality] = {

      def rec(cs: String, acc: Seq[Int], errors: Boolean): Option[Quality] =
        if(errors) { None } else {

          if(cs.isEmpty) Some( Quality(acc) ) else {

            cs.head.toInt match {
              case q if 33 <= q && q <= 126 => rec(cs.tail, acc :+ (q - 33), errors)
              case _                        => rec(cs, acc, errors = true)
            }
          }
        }

      rec(raw, Seq.empty, false)
    }

    val toPhred33: Int => Char = i => (i + 33).toChar
  }

  // TODO value should not have @ at the beginning
  class Id(val value: String) extends AnyVal {

    def asString: String =
      s"@${value}"
  }
  case object Id {

    private val isValid: String => Boolean =
      raw =>
        (raw startsWith "@") &&
        !(raw contains "\n")

    def from(raw: String): Option[Id] = {

      // just *one* line
      val l = raw.filterNot(_ == '\n').dropWhile(_ == '@')

      if(l.nonEmpty) Some( new Id(l) ) else None
    }

    def parseFrom(raw: String): Option[Id] =
      if(isValid(raw)) Some( new Id(raw) ) else None
  }

  case class FASTQ(val id: Id, val sequence: Sequence) {

    def asStringPhred33: String =
      s"${id.asString}\n${sequence.asStringPhred33}"

    def toFASTA: FASTA.Value = FASTA(
      fasta.header( FastaHeader(id.value) )              ::
      fasta.sequence( FastaSequence(sequence.sequence) ) ::
        *[AnyDenotation]
    )
  }

  case object FASTQ {

    def fromStringsPhred33(
      id: String,
      sequence: String,
      quality: String
    )
    : Option[FASTQ] = {

      val z = (Id.from(id), Sequence.fromStringsPhred33(sequence, quality))

      for (a <- z._1; b <- z._2) yield FASTQ(a,b)
    }
  }

  implicit class FASTQIteratorOps(val fastqs: Iterator[FASTQ]) extends AnyVal {

    def appendTo(file: File): File = {

      val wr = new BufferedWriter(new FileWriter(file, true))

      if(fastqs.hasNext) {

        fastqs.foreach { fq => { wr.write( fq.asStringPhred33 ); wr.newLine } }
        wr.close; file
      }
      else file
    }
  }

  implicit class IteratorFASTQOps(val lines: Iterator[String]) extends AnyVal {

    def parseFastqPhred33: Iterator[Option[FASTQ]] =
      lines
        .filterNot(_.isEmpty)
        .grouped(4)
        .map { quartet =>
          if(quartet(3) == "+") {

            Id.parseFrom( quartet(0) ) flatMap { id =>
              Sequence.fromStringsPhred33( quartet(1), quartet(3) ) map { FASTQ(id,_) }
            }
          }
          else None
        }

    def parseFastqPhred33DropErrors: Iterator[FASTQ] =
      parseFastqPhred33 collect { case Some(fq) => fq }
  }

}
