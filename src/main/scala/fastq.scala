package ohnosequences.fastarious

import ohnosequences.cosas._, types._, typeSets._, properties._, records._
import fasta._

case object fastq {

  // id should not include the '@' char
  case object id        extends Property[FastqId]("id") { val start = "@" }
  case object sequence  extends Property[String]("sequence")
  case object plus      extends Property[String]("plus") { val start = "+" }
  case object quality   extends Property[String]("quality")

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

  implicit lazy val plusParser = PropertyParser(plus, plus.label){
    v: String => if (v startsWith plus.start) Some(v.drop(1)) else None
  }

  case object FastqId {

    def apply(i: String): FastqId =
      if(i startsWith id.start) new FastqId(i drop 1) else new FastqId(i)
  }
  // the value here is assumed (and guaranteed) to be '@'-free
  final class FastqId private[fastarious](val value: String) extends AnyVal {

    def toFastaHeader: FastaHeader =
      new FastaHeader(value)

    def asString: String =
      s"${id.start}${value}"
  }

  case class FASTQOps(val seq: FASTQ.Raw) extends AnyVal {

    @inline private def me: ValueOf[FASTQ.type] = FASTQ(seq)

    def toFASTA: ValueOf[FASTA] =
      FASTA(
        fasta.header( me.get(id).value toFastaHeader )                             :~:
        fasta.sequence( FastaLines(Seq( (me get sequence).value )) )  :~: ∅
      )

    def toLines: Seq[String] =
      Seq(
        s"${id.start}${me get id value}",
        me get sequence value,
        me get plus value,
        me get quality value
      )

  }
}
