package ohnosequences.fastarious

import ohnosequences.cosas._, types._, properties._, records._

case object fasta {

  case object header    extends Property[String]("header") { val start = ">" }
  case object sequence  extends Property[FastaLines]("sequence")

  type FASTA = FASTA.type
  case object FASTA extends Record(
    header    :&:
    sequence  :&: □
  )
  {
    implicit def fastaOps(fa: FASTA.type := FASTA.Raw): FASTAOps = new FASTAOps(fa.value)
  }

  implicit lazy val serializeHeader =
    PropertySerializer(header, header.label){ v: String => Some(s"${header.start}${v}") }
  implicit lazy val headerParser =
    PropertyParser(header, header.label){ v: String => if (v startsWith header.start) Some(v.drop(1)) else None }

  implicit lazy val sequenceParser =
    PropertyParser(sequence, sequence.label){ v: String => Some(FastaLines(v)) }

  case object FastaLines {

    def apply(ll: Seq[String]): FastaLines =
      new FastaLines( ll map { _.filterNot(c => c == '\n') } flatMap { _.grouped(70) } )

    def apply(l: String): FastaLines =
      new FastaLines( l.filterNot(c => c == '\n').grouped(70).toSeq )
  }

  final class FastaLines private(val lines: Seq[String]) extends AnyVal {

    def ++(other: FastaLines): FastaLines = FastaLines(lines ++ other.lines)
  }

  final class FASTAOps(val fa: FASTA.Raw) extends AnyVal {

    @inline private def me: ValueOf[FASTA.type] = FASTA(fa)

    def toLines: Seq[String] =
      Seq(s"${header.start}${me get header value}") ++ ((me get sequence value) lines)
  }

}
