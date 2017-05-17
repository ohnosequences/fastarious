
```scala
package ohnosequences.fastarious

import ohnosequences.cosas._, types._, records._, fns._, klists._
import java.io._
```


# FASTA

Fasta is a not so well-defined format, consisting on

1. a header
2. and a sequence

As usual, it is tied with a particular representation in the form of text files, where a (multi)set of elements consists of

```
>header1
sequence1
>header2
sequence2
```

The header *must* be **one** line, while sequences are split into `70` characters by line (the good old fixed-width terminal times).

I know all this is crazy, but that's the way bioinformatics is.


```scala
case object fasta {

  case object header extends Type[FastaHeader]("header") {

    val start: String = ">"

    def is(str: String): Boolean = str startsWith start
  }
  case object sequence  extends Type[FastaSequence]("sequence") {

    def is(str: String): Boolean = ! header.is(str)
  }
```


The `FASTA` record is what you use for sane interaction with fasta elements.


```scala
  type FASTA = FASTA.type
  case object FASTA extends RecordType(
    header    :×:
    sequence  :×:
    |[AnyType]
  ) {
    type RealRaw =
      (header.type := FastaHeader) ::
      (sequence.type := FastaSequence) ::
      *[AnyDenotation]

    type Value = FASTA := RealRaw

    implicit def fastaOps(fa: FASTA.Value): FASTAOps = new FASTAOps(fa)
    implicit def fastaIteratorOps(fas: Iterator[FASTA.Value]): FASTAIteratorOps = new FASTAIteratorOps(fas)
  }

  final class FASTAIteratorOps(val fastas: Iterator[FASTA.Value]) extends AnyVal {

    def appendTo(file: File) = {

      val wr = new BufferedWriter(new FileWriter(file, true))

      fastas.foreach { fa => { wr.write( fa.asString ); wr.newLine } }

      wr.close
    }
  }


  case object FastaHeader {

    def apply(h: String): FastaHeader = new FastaHeader(h.stripPrefix(header.start))
  }

  final class FastaHeader private[fastarious](val value: String) extends AnyVal {

    override def toString: String = s"${header.start}${value}"

    final def id: String = value.takeWhile(_ != ' ')
```

Note that description will keep the initial empty space, so that `value == s"${id}${description}"`

```scala
    final def description: String = value.stripPrefix(id)
  }

  case object FastaSequence {

    def apply(ll: Seq[String]): FastaSequence =
      new FastaSequence( ll.map(utils.removeAllSpace).mkString )

    def apply(l: String): FastaSequence =
      new FastaSequence( utils.removeAllSpace(l) )
  }

  final class FastaSequence private (val value: String) extends AnyVal {

    final def ++(other: FastaSequence): FastaSequence =
      FastaSequence(s"${value}${other.value}")
  }

  final class FASTAOps(val fa: FASTA.Value) extends AnyVal {

    // instead of toLines, which is a confusing name anyway

    def toMap: Map[String, String] = Map(
      header.label   -> (fa getV header).toString,
      sequence.label -> (fa getV sequence).value.grouped(70).mkString("\n")
    )

    def asString: String = toMap.values.mkString("\n")
    def lines: Seq[String] =(fa getV header).toString :: (fa getV sequence).value.grouped(70).toList
  }
```


## Fasta parsing and serialization

These are parser and serializers instances for the `FASTA` record field. They make possible transparent use of *cosas* record parsing from `Map[String,String]`.


```scala
  implicit lazy val headerSerializer =
    new DenotationSerializer(header, header.label)({ h: FastaHeader => Some(h.toString) })
  implicit lazy val headerParser =
    new DenotationParser(header, header.label)({ v: String => Some(FastaHeader(v)) })

  implicit lazy val sequenceSerializer =
    new DenotationSerializer(sequence, sequence.label)({ fl: FastaSequence => Some(fl.value) })
  implicit lazy val sequenceParser =
    new DenotationParser(sequence, sequence.label)({ v: String => Some(FastaSequence(v)) })
```

Note, all we only use `next`, `hasNext` and `head` method here, so the `lines` iterator can be used after calling any of these extension methods and can be considered as the parsing "remainder" (i.e. unparsed part)

```scala
  implicit class BufferedIteratorFASTAOps(val lines: BufferedIterator[String]) extends AnyVal {
```

Similar to `takeWhile`, but the iterator **can be reused** after

```scala
    def cutUntil(condition: String => Boolean): Seq[String] = {

      @annotation.tailrec
      def rec(acc: collection.mutable.Buffer[String]): collection.mutable.Buffer[String] = {
        if (lines.hasNext && !condition(lines.head))
          rec(acc :+ lines.next())
        else acc
      }

      rec(collection.mutable.Buffer.empty)
    }
```

Use this method to parse all FASTA values from the `lines` iterator (with possible errors)

```scala
    def parseFasta():
        Iterator[ Either[ParseDenotationsError, FASTA.Value] ] =
    new Iterator[ Either[ParseDenotationsError, FASTA.Value] ] {
```

If there is one more header, there is one more FASTA value (even if the sequence is empty)

```scala
      def hasNext: Boolean = lines.hasNext && fasta.header.is(lines.head)

      def next(): Either[ParseDenotationsError, FASTA.Value] = {
        val hdr: String = lines.next()
        val seq: String = cutUntil(fasta.header.is).mkString

        val valueMap = Map[String, String](
            header.label -> hdr,
          sequence.label -> seq
        )

        FASTA parse valueMap
      }
    }
```

This is the same as `parseFasta` dropping all erroneous FASTAs and skipping anything before the first FASTA value

```scala
    def parseFastaDropErrors(skipCrap: Boolean = true): Iterator[FASTA.Value] = {
      if (skipCrap) cutUntil(fasta.header.is)
      parseFasta() collect { case Right(fa) => fa }
    }
  }

}

```




[test/scala/DNA.scala]: ../../test/scala/DNA.scala.md
[test/scala/NcbiHeadersTests.scala]: ../../test/scala/NcbiHeadersTests.scala.md
[test/scala/FastqTests.scala]: ../../test/scala/FastqTests.scala.md
[test/scala/FastaTests.scala]: ../../test/scala/FastaTests.scala.md
[test/scala/QualityScores.scala]: ../../test/scala/QualityScores.scala.md
[main/scala/DNAQ.scala]: DNAQ.scala.md
[main/scala/qualityScores.scala]: qualityScores.scala.md
[main/scala/DNA.scala]: DNA.scala.md
[main/scala/fasta.scala]: fasta.scala.md
[main/scala/fastq.scala]: fastq.scala.md
[main/scala/SequenceQuality.scala]: SequenceQuality.scala.md
[main/scala/utils.scala]: utils.scala.md
[main/scala/sequence.scala]: sequence.scala.md
[main/scala/ncbiHeaders.scala]: ncbiHeaders.scala.md