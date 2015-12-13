
```scala
package ohnosequences.fastarious

import ohnosequences.cosas._, types._, records._, fns._, klists._

case object fasta {
```

values of this property correspond to the header

```scala
  case object header    extends Type[FastaHeader]("header") { val start = ">" }
  case object sequence  extends Type[FastaLines]("sequence")

  type FASTA = FASTA.type
  case object FASTA extends RecordType(
    header    :×:
    sequence  :×: |[AnyType]
  )
  {
    implicit def fastaOps[RV <: FASTA.Raw](fa: FASTA := RV): FASTAOps[RV] = new FASTAOps[RV](fa.value)
  }

  implicit lazy val headerSerializer =
    new DenotationSerializer(header, header.label)({ h: FastaHeader => Some(h.asString) })
  implicit lazy val headerParser =
    new DenotationParser(header, header.label)({ v: String => Some(FastaHeader(v)) })

  implicit lazy val sequenceSerializer =
    new DenotationSerializer(sequence, sequence.label)({ fl: FastaLines => Some(fl.asString) })
  implicit lazy val sequenceParser =
    new DenotationParser(sequence, sequence.label)({ v: String => Some(FastaLines(v)) })

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

  final class FASTAOps[RV <: FASTA.Raw](val fa: RV) extends AnyVal {

    @inline private def me: FASTA := RV = FASTA(fa)

    def toLines(implicit
      getH: AnyApp1At[findS[AnyDenotation { type Tpe = header.type }],RV] { type Y = header.type := header.Raw },
      getS: AnyApp1At[findS[AnyDenotation { type Tpe = sequence.type }],RV] { type Y = sequence.type := sequence.Raw }
    )
    : Seq[String] =
      // header value
      Seq(s"${header.start}${me.getV(header).value}") ++ me.getV(sequence).lines
  }

}

```




[test/scala/FastqTests.scala]: ../../test/scala/FastqTests.scala.md
[test/scala/FastaTests.scala]: ../../test/scala/FastaTests.scala.md
[main/scala/fasta.scala]: fasta.scala.md
[main/scala/fastq.scala]: fastq.scala.md
[main/scala/utils.scala]: utils.scala.md