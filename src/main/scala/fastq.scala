package ohnosequences.fastarious

import fasta._
import ohnosequences.cosas._, types._, records._, fns._, klists._
import java.io._

case object fastq {

  /*

    # sequence

    A fastq sequence has the sequence itself plus the corresponding quality. They are checked at construction.
  */
  case class seq private[fastarious] (val sequence: String, val quality: phred33) {

    def length =
      sequence.length

    def drop(n: Int): seq =
      seq( sequence drop n, phred33( quality.value drop n ) )

    def dropRight(n: Int): seq =
      seq( sequence dropRight n, phred33( quality.value dropRight n ) )

    def slice(from: Int, until: Int): seq =
      seq( sequence.slice(from, until), phred33( quality.value.slice(from, until) ) )

    def ++(other: seq): seq =
      seq(sequence ++ other.sequence, phred33( quality.value ++ other.quality.value ))

    def asString: String =
      s"${sequence}\n+\n${quality.value}"
  }

  case object seq {

    private def from(rawSeq: String, rawQual: phred33): Option[seq] =
      if(rawSeq.length == rawQual.value.length) Some(seq(rawSeq, rawQual)) else None

    def from(rawSeq: String, rawQual: String): Option[seq] =
      (phred33 from rawQual) flatMap { from(rawSeq, _) }
  }

  case class  phred33 private[fastarious] (val value: String) extends AnyVal
  case object phred33 {

    def from(raw: String): Option[phred33] =
      if(raw forall isValid) Some(phred33(raw)) else None

    val isValid: Char => Boolean =
      c => 33 <= c.toInt && c.toInt <= 126
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
      val l = raw.filterNot(_ == '\n').dropWhile(_ == "@")

      if(l.nonEmpty) Some( new Id(l) ) else None
    }

    def parseFrom(raw: String): Option[Id] =
      if(isValid(raw)) Some( new Id(raw) ) else None
  }

  case class FASTQ(val id: Id, val sequence: seq) {

    def asString: String =
      s"${id.asString}\n${sequence.asString}"

    def toFASTA: FASTA.Value = FASTA(
      fasta.header( FastaHeader(id.value) )              ::
      fasta.sequence( FastaSequence(sequence.sequence) ) ::
        *[AnyDenotation]
    )
  }

  case object FASTQ {

    def from(
      id: String,
      sequence: String,
      quality: String
    )
    : Option[FASTQ] = {

      val z = (Id.from(id), seq.from(sequence, quality))

      for (a <- z._1; b <- z._2) yield FASTQ(a,b)
    }
  }

  implicit class FASTQIteratorOps(val fastqs: Iterator[FASTQ]) extends AnyVal {

    def appendTo(file: File): File = {

      val wr = new BufferedWriter(new FileWriter(file, true))

      if(fastqs.hasNext) {

        fastqs.foreach { fq => { wr.write( fq.asString ); wr.newLine } }
        wr.close; file
      }
      else file
    }
  }

  implicit class IteratorFASTQOps(val lines: Iterator[String]) extends AnyVal {

    def parseFastq: Iterator[Option[FASTQ]] =
      lines
        .filterNot(_.isEmpty)
        .grouped(4)
        .map { quartet =>
          if(quartet(3) == "+") {

            Id.parseFrom( quartet(0) ) flatMap { id =>
              seq.from( quartet(1), quartet(3) ) map { FASTQ(id,_) }
            }
          }
          else None
        }

    def parseFastqDropErrors: Iterator[FASTQ] =
      parseFastq collect { case Some(fq) => fq }
  }

}
