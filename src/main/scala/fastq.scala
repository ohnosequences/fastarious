package ohnosequences.fastarious

import ohnosequences.cosas._, types._, records._, fns._, klists._
import fasta._

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
  )
  {
    implicit def fastqOps[RV <: FASTQ.Raw](fq: FASTQ.type := RV): FASTQOps[RV] = FASTQOps(fq.value)

    import better.files.File
    implicit class FASTQIteratorOps(val fastqs: Iterator[
        FASTQ.type := (
          (id.type        := id.Raw)        ::
          (sequence.type  := sequence.Raw)  ::
          (plus.type      := plus.Raw)      ::
          (quality.type   := quality.Raw)   ::
          *[AnyDenotation]
        )
      ]
    ) extends AnyVal {

      def appendTo(file: File) = {

        import java.io._
        val wr = new BufferedWriter(new FileWriter(file.toJava, true))

        fastqs.foreach { fa => { wr.write( fa.toLines ); wr.newLine } }

        wr.close
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
  final class FastqId private[fastarious](val value: String) extends AnyVal {

    def toFastaHeader: FastaHeader =
      new FastaHeader(value)

    def asString: String =
      s"${id.start}${value}"
  }

  case object FastqSequence {

    def apply(s: String): FastqSequence =
      new FastqSequence( utils.removeAllSpace(s) )
  }
  final class FastqSequence private (val value: String) extends AnyVal {

    def asString = value
  }

  case object FastqPlus {

    def apply(i: String): FastqPlus = {

      // just *one* line
      val l = i.filterNot(_ == '\n')
      if(l startsWith plus.start) new FastqPlus(l drop 1) else new FastqPlus(l)
    }
  }
  // the value here is assumed (and guaranteed) to be '@'-free
  final class FastqPlus private[fastarious](val value: String) extends AnyVal {

    def asString: String =
      s"${plus.start}${value}"
  }

  case object FastqQuality {

    def apply(s: String): FastqQuality =
      new FastqQuality( utils.removeAllSpace(s) )
  }
  final class FastqQuality private (val value: String) extends AnyVal {

    def asString = value
  }

  case class FASTQOps[RV <: FASTQ.Raw](val seq: RV) extends AnyVal {

    @inline private def me: FASTQ := RV =
      FASTQ(seq)

    def toFASTA(implicit
      getId: AnyApp1At[findS[AnyDenotation.Of[id.type]], RV] { type Y = id.type := id.Raw },
      getSeq: AnyApp1At[findS[AnyDenotation.Of[sequence.type]], RV] { type Y = sequence.type := sequence.Raw }
    )
    : FASTA := ((fasta.header.type := fasta.header.Raw) :: (fasta.sequence.type := fasta.sequence.Raw) :: *[AnyDenotation]) =
      FASTA(
        fasta.header( me.getV(id).toFastaHeader )                   ::
        fasta.sequence( FastaSequence(me.getV(sequence).asString) ) :: *[AnyDenotation]
      )

    def toLines(implicit
      getId   : AnyApp1At[findS[AnyDenotation.Of[id.type]], RV] { type Y = id.type := id.Raw },
      getSeq  : AnyApp1At[findS[AnyDenotation.Of[sequence.type]], RV] { type Y = sequence.type := sequence.Raw },
      getPlus : AnyApp1At[findS[AnyDenotation.Of[plus.type]], RV] { type Y = plus.type := plus.Raw },
      getQual : AnyApp1At[findS[AnyDenotation.Of[quality.type]],RV] { type Y = quality.type := quality.Raw }
    )
    : String =
    s"${(me getV id).asString}\n${(me getV sequence).asString}\n${(me getV plus).asString}\n${(me getV quality).asString}"
  }

  def parseMapFromLines(lines: Iterator[String]): Iterator[Map[String, String]] = {

    // NOTE much unsafe, should check for id and qual chars etc
    lines.grouped(4) map {
      quartet => {
        Map(
          id.label        -> quartet(0),
          sequence.label  -> quartet(1),
          plus.label      -> quartet(2),
          quality.label   -> quartet(3)
        )
      }
    }
  }

  def parseFastqFromLines(lines: Iterator[String])
  : Iterator[
      Either[
        ParseDenotationsError,
        FASTQ.type := (
          (id.type        := id.Raw)        ::
          (sequence.type  := sequence.Raw)  ::
          (plus.type      := plus.Raw)      ::
          (quality.type   := quality.Raw)   ::
          *[AnyDenotation]
        )
      ]
    ]
  = parseMapFromLines(lines) map { strMap => FASTQ parse strMap }

}
