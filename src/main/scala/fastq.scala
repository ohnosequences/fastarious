package ohnosequences.fastarious

import ohnosequences.cosas._, properties._, records._

case object fastq {

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
}
