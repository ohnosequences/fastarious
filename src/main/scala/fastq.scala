package ohnosequences.fastarious

import ohnosequences.cosas._, types._, typeSets._, properties._, records._
import fasta._

case object fastq {

  case object +
  // id should not include the '@' char
  case object id        extends Property[String]("fastq.id") { val start = "@" }
  case object sequence  extends Property[String]("fastq.sequence")
  case object plus      extends Property[String]("fastq.plus")
  case object quality   extends Property[String]("fastq.quality")

  case object FASTQ extends Record(
    id        :&:
    sequence  :&:
    plus      :&:
    quality   :&: □
  )
  {
    implicit def fastqOps(fq: FASTQ.type := FASTQ.Raw): FASTQOps = FASTQOps(fq.value)
  }



  implicit lazy val idSerializer    = PropertySerializer(id, id.label){ v => Some(s"${id.start}${v}") }
  implicit lazy val plusSerializer  = PropertySerializer(plus, plus.label){ v => Some("+") }

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
