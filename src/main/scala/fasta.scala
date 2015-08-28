package ohnosequences.fastarious

import ohnosequences.cosas._, properties._, records._

case object fasta {

  case object header    extends Property[String]("fasta.header")
  case object sequence  extends Property[FastaLines]("fasta.sequence")

  case object FASTA extends Record(
    header    :&:
    sequence  :&: □
  )

  case object FastaLines {

    def apply(ll: Seq[String]): FastaLines = new FastaLines(ll flatMap { _.grouped(70) })
  }

  class FastaLines private(val lines: Seq[String]) extends AnyVal {

    def ++(other: FastaLines): FastaLines = FastaLines(lines ++ other.lines)
  }

}
