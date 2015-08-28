package ohnosequences.fastarious

import ohnosequences.cosas._, types._, typeSets._, properties._, records._
import fasta._

case object fastq {

  // id should not include the '@' char
  case object id        extends Property[String]("id") { val start = "@" }
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

  implicit lazy val idSerializer = PropertySerializer(id, id.label){ v => Some(s"${id.start}${v}") }
  // checks the first char
  implicit lazy val idParser = PropertyParser(id, id.label){
    v: String => if (v startsWith id.start) Some(v.drop(1)) else None
  }
  implicit lazy val plusParser = PropertyParser(plus, plus.label){
    v: String => if (v startsWith plus.start) Some(v.drop(1)) else None
  }

  case class FASTQOps(val seq: FASTQ.Raw) extends AnyVal {

    @inline private def me: ValueOf[FASTQ.type] = FASTQ(seq)

    def toFASTA: ValueOf[FASTA] =
      FASTA(
        fasta.header( (me get id).value )                             :~:
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
