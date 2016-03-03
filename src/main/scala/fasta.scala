package ohnosequences.fastarious

import ohnosequences.cosas._, types._, records._, fns._, klists._

/*
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
*/
case object fasta {

  case object header extends Type[FastaHeader]("header") {

    val start: String = ">"

    def is(str: String): Boolean = str startsWith start
  }
  case object sequence  extends Type[FastaSequence]("sequence") {

    def is(str: String): Boolean = ! header.is(str)
  }

  /*
    The `FASTA` record is what you use for sane interaction with fasta elements.
  */
  type FASTA = FASTA.type
  case object FASTA extends RecordType(
    header    :×:
    sequence  :×:
    |[AnyType]
  )
  {
    // TODO after updating cosas, remove the RV param; the FASTA.Raw type is fixed
    implicit def fastaOps[RV <: FASTA.Raw](fa: FASTA := RV): FASTAOps[RV] = new FASTAOps[RV](fa.value)
    
    import better.files._
    implicit class FASTAIteratorOps(val fastas: Iterator[
        FASTA.type := (
          (header.type := FastaHeader)      ::
          (sequence.type := FastaSequence)  ::
          *[AnyDenotation]
        )
      ]
    ) extends AnyVal {

      def appendTo(file: File) = {

        import java.io._
        val wr = new BufferedWriter(new FileWriter(file.toJava, true))

        fastas.foreach { fa => { wr.write( fa.toLines ); wr.newLine } }

        wr.close
      }
    }
  }

  case object FastaHeader {

    def apply(h: String): FastaHeader =
      if(h startsWith header.start) new FastaHeader(h drop 1) else new FastaHeader(h)
  }

  final class FastaHeader private[fastarious](val value: String) extends AnyVal {

    final def asString: String = s"${header.start}${value}"

    final def id: String = value.takeWhile(_ != ' ')
    final def description: String = value.stripPrefix(id)
  }

  case object FastaSequence {

    def apply(ll: Seq[String]): FastaSequence =
      new FastaSequence( ll.map(utils.removeAllSpace).mkString )

    def apply(l: String): FastaSequence =
      new FastaSequence( utils.removeAllSpace(l) )
  }

  final class FastaSequence private(val lines: String) extends AnyVal {

    final def ++(other: FastaSequence): FastaSequence =
      FastaSequence(s"${lines}${other.lines}")
  }

  final class FASTAOps[RV <: FASTA.Raw](val fa: RV) extends AnyVal {

    @inline private def me: FASTA := RV = FASTA(fa)

    // TODO remove all the implicits once we have a better FASTA.Raw type
    def toLines(implicit
      getH: AnyApp1At[findS[AnyDenotation { type Tpe = header.type }],RV] { type Y = header.type := header.Raw },
      getS: AnyApp1At[findS[AnyDenotation { type Tpe = sequence.type }],RV] { type Y = sequence.type := sequence.Raw }
    )
    : String = s"${fa.head.value.asString}\n${fa.tail.head.value.lines.grouped(70).mkString("\n")}"
  }

  /*
    ## Fasta parsing and serialization

    These are parser and serializers instances for the `FASTA` record field. They make possible transparent use of *cosas* record parsing from `Map[String,String]`.
  */
  implicit lazy val headerSerializer =
    new DenotationSerializer(header, header.label)({ h: FastaHeader => Some(h.asString) })
  implicit lazy val headerParser =
    new DenotationParser(header, header.label)({ v: String => Some(FastaHeader(v)) })

  implicit lazy val sequenceSerializer =
    new DenotationSerializer(sequence, sequence.label)({ fl: FastaSequence => Some(fl.lines) })
  implicit lazy val sequenceParser =
    new DenotationParser(sequence, sequence.label)({ v: String => Some(FastaSequence(v)) })

  /*
    This method returns an iterator over `Map[String, String]` which can be directly used by `FASTA.parse`.

    **NOTE** the `lines` iterator should *not* be used after calling `parseFromLines` on it.
  */
  final def parseMapFromLines(lines: Iterator[String]): Iterator[Map[String, String]] = new Iterator[Map[String, String]] {

    // NOTE see https://groups.google.com/forum/#!topic/scala-user/BPjFbrglfMs for why this is that ugly
    def hasNext = lines.hasNext

    var isFirst: Boolean = true
    var currentHeader: String = ""

    def next() = {

      val currentLines = new StringBuilder
      if(isFirst) { isFirst = false; currentHeader = lines.next }

      import util.control.Breaks._

      breakable {
        while (lines.hasNext) {
          val line = lines.next
          if(sequence is line)
            currentLines append line
          else {
            currentHeader = line; break
          }
        }
      }

      collection.immutable.HashMap(
        header.label    -> currentHeader,
        sequence.label  -> currentLines.toString
      )
    }
  }

  /*
    Exactly the same as `parseMapFromLines`, but returning either a parsing error or a `FASTA` denotation.
  */
  // TODO update after gettting good Raw in cosas records
  final def parseFastaFromLines(lines: Iterator[String])
  : Iterator[
      Either[
        ParseDenotationsError,
        FASTA.type := (
          (header.type := FastaHeader)      ::
          (sequence.type := FastaSequence)  ::
          *[AnyDenotation]
        )
      ]
    ]
  = parseMapFromLines(lines) map { strMap => FASTA parse strMap }
}
