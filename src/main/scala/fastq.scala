package ohnosequences.fastarious

import ohnosequences.cosas._, types._, typeSets._, properties._, records._
import fasta._

case object fastq {

  // id should not include the '@' char
  case object id        extends Property[FastqId]("id") { val start = "@" }
  case object sequence  extends Property[FastqSequence]("sequence")
  case object plus      extends Property[FastqPlus]("plus") { val start = "+" }
  case object quality   extends Property[FastqQuality]("quality")

  case object FASTQ extends Record(
    id        :&:
    sequence  :&:
    plus      :&:
    quality   :&: □
  )
  {
    implicit def fastqOps(fq: FASTQ.type := FASTQ.Raw): FASTQOps = FASTQOps(fq.value)
  }

  implicit lazy val idSerializer =
    PropertySerializer(id, id.label){ v => Some(v asString) }
  implicit lazy val idParser =
    PropertyParser(id, id.label){ v: String => Some(FastqId(v)) }

  implicit lazy val plusParser =
    PropertyParser(plus, plus.label){ v: String => Some(FastqPlus(v)) }

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
  final class FastqSequence private (val value: String) extends AnyVal

  case object FastqPlus {

    def apply(i: String): FastqPlus = {

      // just *one* line
      val l = i.filterNot(_ == '\n')
      if(l startsWith plus.start) new FastqPlus(l drop 1) else new FastqPlus(l)
    }
  }
  // the value here is assumed (and guaranteed) to be '@'-free
  final class FastqPlus private[fastarious](val value: String) extends AnyVal {

    def toFastaHeader: FastaHeader =
      new FastaHeader(value)

    def asString: String =
      s"${id.start}${value}"
  }

  case object FastqQuality {

    def apply(s: String): FastqQuality =
      new FastqQuality( utils.removeAllSpace(s) )
  }
  final class FastqQuality private (val value: String) extends AnyVal



  case class FASTQOps(val seq: FASTQ.Raw) extends AnyVal {

    @inline private def me: ValueOf[FASTQ.type] = FASTQ(seq)

    def toFASTA: ValueOf[FASTA] =
      FASTA(
        fasta.header( me.get(id).value toFastaHeader )                             :~:
        fasta.sequence( FastaLines((me get sequence).value.value) )  :~: ∅
      )

    def toLines: Seq[String] =
      Seq(
        (me get id value).asString,
        (me get sequence value).value,
        (me get plus value).asString,
        (me get quality value).value
      )
  }
}
