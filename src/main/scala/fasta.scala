package ohnosequences.fastarious

import ohnosequences.cosas._, types._, records._

case object fasta {

  /* values of this property correspond to the header */
  case object header    extends Property[FastaHeader]("header") { val start = ">" }
  case object sequence  extends Property[FastaLines]("sequence")

  type FASTA = FASTA.type
  case object FASTA extends Record(
    header    :&:
    sequence  :&: □
  )
  {
    implicit def fastaOps(fa: FASTA.type := FASTA.Raw): FASTAOps = new FASTAOps(fa.value)
  }

  implicit lazy val headerSerializer =
    PropertySerializer(header, header.label){ h: FastaHeader => Some(h.asString) }
  implicit lazy val headerParser =
    PropertyParser(header, header.label){ v: String => Some(FastaHeader(v)) }

  implicit lazy val sequenceSerializer =
    PropertySerializer(sequence, sequence.label){ fl: FastaLines => Some(fl.asString) }
  implicit lazy val sequenceParser =
    PropertyParser(sequence, sequence.label){ v: String => Some(FastaLines(v)) }

  case object FastaHeader {

    def apply(h: String): FastaHeader =
      if(h startsWith header.start) new FastaHeader(h drop 1) else new FastaHeader(h)
  }

  final class FastaHeader private[fastarious](val value: String) extends AnyVal {

    def asString: String = s"${header.start}${value}"
  }

  case object FastaLines {

    def apply(ll: Seq[String]): FastaLines =
      new FastaLines( (ll map utils.removeAllSpace) flatMap { _.grouped(70) } )

    def apply(l: String): FastaLines =
      new FastaLines( utils.removeAllSpace(l).grouped(70).toList )
  }

  final class FastaLines private(val lines: Seq[String]) extends AnyVal {

    // this potentially creates funny albeit correct line length at the joining lining
    def ++(other: FastaLines): FastaLines =
      FastaLines(lines ++ other.lines)

    def asString: String =
      lines.mkString
  }

  final class FASTAOps(val fa: FASTA.Raw) extends AnyVal {

    @inline private def me: ValueOf[FASTA.type] = FASTA(fa)

    def toLines: Seq[String] =
      // header value
      Seq(s"${header.start}${me.getV(header).value}") ++ me.getV(sequence).lines
  }

}
