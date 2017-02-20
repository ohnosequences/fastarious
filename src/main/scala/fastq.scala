package ohnosequences.fastarious

import fasta._
import ohnosequences.cosas._, types._, records._, fns._, klists._
import java.io._

case object fastq {

  // id should not include the '@' char
  case object id        extends Type[FastqId]("id") { val start = "@" }
  case object sequence  extends Type[FastqSequence]("sequence")
  case object plus      extends Type[FastqPlus]("plus") { val start = "+" }
  case object quality   extends Type[FastqQuality]("quality")

  type FASTQ = FASTQ.type
  case object FASTQ extends RecordType(
    id        :×:
    sequence  :×:
    plus      :×:
    quality   :×:
    |[AnyType]
  ) {

    type RealRaw =
      (id.type        := id.Raw)        ::
      (sequence.type  := sequence.Raw)  ::
      (plus.type      := plus.Raw)      ::
      (quality.type   := quality.Raw)   ::
      *[AnyDenotation]

    type Value = FASTQ := RealRaw

    implicit def fastqOps(fq: Value): FASTQOps = FASTQOps(fq)

    implicit class FASTQIteratorOps(val fastqs: Iterator[Value]) extends AnyVal {

      def appendTo(file: File): File = {

        val wr = new BufferedWriter(new FileWriter(file, true))

        if(fastqs.hasNext) {

          fastqs.foreach { fq => { wr.write( fq.asString ); wr.newLine } }
          wr.close; file
        } else file
      }
    }
  }

  implicit lazy val idSerializer =
    new DenotationSerializer(id, id.label)({ v: FastqId => Some(v asString) })
  implicit lazy val idParser =
    new DenotationParser(id, id.label)({ v: String => Some(FastqId(v)) })

  implicit lazy val sequenceSerializer =
    new DenotationSerializer(sequence, sequence.label)({ v: FastqSequence => Some(v asString) })
  implicit lazy val sequenceParser =
    new DenotationParser(sequence, sequence.label)({ v: String => Some(FastqSequence(v)) })

  implicit lazy val qualitySerializer =
    new DenotationSerializer(quality, quality.label)({ v: FastqQuality => Some(v asString) })
  implicit lazy val qualityParser =
    new DenotationParser(quality, quality.label)({ v: String => Some(FastqQuality(v)) })

  implicit lazy val plusSerializer =
    new DenotationSerializer(plus, plus.label)({ v: FastqPlus => Some(v asString) })
  implicit lazy val plusParser =
    new DenotationParser(plus, plus.label)({ v: String => Some(FastqPlus(v)) })

  case object FastqId {

    def apply(i: String): FastqId = {

      // just *one* line
      val l = i.filterNot(_ == '\n')
      if(l startsWith id.start) new FastqId(l drop 1) else new FastqId(l)
    }
  }
  // the value here is assumed (and guaranteed) to be '@'-free
  final class FastqId private[fastarious] (val value: String) extends AnyVal {

    def toFastaHeader: FastaHeader = new FastaHeader(value)

    private[fastarious] def asString: String = s"${id.start}${value}"
  }

  case object FastqSequence {

    def apply(s: String): FastqSequence =
      new FastqSequence( utils.removeAllSpace(s) )
  }
  final class FastqSequence private (val value: String) extends AnyVal {

    def asString: String = value

    def drop(n: Int): FastqSequence =
      FastqSequence( value drop n )

    def dropRight(n: Int): FastqSequence =
      FastqSequence( value dropRight n )

    def slice(from: Int, until: Int): FastqSequence =
      FastqSequence( value.slice(from, until) )

    def length: Int =
      value.length
  }

  case object FastqPlus {

    def apply(i: String): FastqPlus = {

      // just *one* line
      val l = i.filterNot(_ == '\n')
      if(l startsWith plus.start) new FastqPlus(l drop 1) else new FastqPlus(l)
    }
  }
  // the value here is assumed (and guaranteed) to be '@'-free
  final class FastqPlus private[fastarious] (val value: String) extends AnyVal {

    private[fastarious] def asString: String = s"${plus.start}${value}"
  }

  case object FastqQuality {

    def apply(s: String): FastqQuality =
      new FastqQuality( utils.removeAllSpace(s) )
  }
  final class FastqQuality private (val value: String) extends AnyVal {

    private[fastarious] def asString: String = value

    def drop(n: Int): FastqQuality =
      FastqQuality( value drop n )

    def dropRight(n: Int): FastqQuality =
      FastqQuality( value dropRight n )

    def slice(from: Int, until: Int): FastqQuality =
      FastqQuality( value.slice(from, until) )

    def length: Int =
      value.length
  }

  /*
    ## FASTQ value ops


  */
  case class FASTQOps(val seq: FASTQ.Value) extends AnyVal {

    def toFASTA: FASTA.Value = FASTA(
      fasta.header( seq.getV(id).toFastaHeader ) ::
      fasta.sequence( FastaSequence(seq.getV(sequence).asString) ) ::
      *[AnyDenotation]
    )

    def drop(n: Int): FASTQ.Value =
      seq update {
        ( sequence  := { seq.getV(sequence) drop n } ) ::
        ( quality   := { seq.getV(quality)  drop n } ) ::
          *[AnyDenotation]
      }

    def dropRight(n: Int): FASTQ.Value =
      seq update {
        ( sequence  := { seq.getV(sequence) dropRight n } ) ::
        ( quality   := { seq.getV(quality)  dropRight n } ) ::
          *[AnyDenotation]
      }

    def slice(from: Int, until: Int): FASTQ.Value =
      seq update {
        ( sequence  := { seq.getV(sequence) .slice(from, until) } ) ::
        ( quality   := { seq.getV(quality)  .slice(from, until) } ) ::
          *[AnyDenotation]
      }

    def length: Int =
      (seq getV sequence).length

    def asString: String = Seq(
      seq.getV(id).asString,
      seq.getV(sequence).asString,
      seq.getV(plus).asString,
      seq.getV(quality).asString
    ).mkString("\n")
  }

  implicit class IteratorFASTQOps(val lines: Iterator[String]) extends AnyVal {

    def parseMap(): Iterator[Map[String, String]] = {

      // NOTE much unsafe, should check for id and qual chars etc
      lines
        .filterNot(_.isEmpty)
        .grouped(4)
        .map { quartet =>
          Map(
            id.label       -> quartet(0),
            sequence.label -> quartet(1),
            plus.label     -> quartet(2),
            quality.label  -> quartet(3)
          )
        }
    }

    def parseFastq(): Iterator[ Either[ParseDenotationsError, FASTQ.Value] ] =
      parseMap map { strMap => FASTQ parse strMap }

    def parseFastqDropErrors(): Iterator[FASTQ.Value] =
      parseFastq collect { case Right(fq) => fq }
  }

}
