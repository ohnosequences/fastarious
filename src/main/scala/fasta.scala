package ohnosequences.fastarious

import ohnosequences.cosas._, types._, records._, fns._, klists._
import java.io._

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

  case class FASTA(
    val header: Header
    val sequence: Sequence
  )

  case class Header private[fastarious] (val value: String) extends AnyVal {

    override def toString: String = s">${value}"

    final def id: String = value.takeWhile(_ != ' ')

    /* Note that description will keep the initial empty space, so that `value == s"${id}${description}"` */
    final def description: String = value.stripPrefix(id)
  }

  case object Header {

    def from(raw: String): Option[Header] = {
      if (isValid(raw)) Some(Header(h.stripPrefix(">")))
      else None

    def isValid(str: String): Boolean = str startsWith ">"
  }

  case class Sequence private (val value: String) extends AnyVal {

    final def ++(other: Sequence): Sequence =
      Sequence(s"${value}${other.value}")
  }

  case object Sequence {

    def apply(ll: Seq[String]): Sequence =
      new Sequence( ll.map(utils.removeAllSpace).mkString )

    def apply(l: String): Sequence =
      new Sequence( utils.removeAllSpace(l) )

    // def isValid(str: String): Boolean = ! header.is(str)
  }

  implicit class FASTAOps(val fa: FASTA.Value) extends AnyVal {

    // instead of toLines, which is a confusing name anyway

    def toMap: Map[String, String] = Map(
      header.label   -> (fa.header).toString,
      sequence.label -> (fa.sequence).value.grouped(70).mkString("\n")
    )

    def asString: String = toMap.values.mkString("\n")
    def lines: Seq[String] = (fa.header).toString :: (fa.sequence).value.grouped(70).toList
  }



  implicit class FASTAIteratorOps(val fastas: Iterator[FASTA.Value]) extends AnyVal {

    def appendTo(file: File): File = {

      val wr = new BufferedWriter(new FileWriter(file, true))
      fastas.foreach { fa =>
        wr.write( fa.asString )
        wr.newLine
      }
      wr.close

      file
    }
  }

  /* Note, all we only use `next`, `hasNext` and `head` method here, so the `lines` iterator can be used after calling any of these extension methods and can be considered as the parsing "remainder" (i.e. unparsed part) */
  implicit class BufferedIteratorFASTAOps(val lines: BufferedIterator[String]) extends AnyVal {

    /* Similar to `takeWhile`, but the iterator **can be reused** after */
    def cutUntil(condition: String => Boolean): Seq[String] = {

      @annotation.tailrec
      def rec(acc: collection.mutable.Buffer[String]): collection.mutable.Buffer[String] = {
        if (lines.hasNext && !condition(lines.head))
          rec(acc :+ lines.next())
        else acc
      }

      rec(collection.mutable.Buffer.empty)
    }

    /* Use this method to parse all FASTA values from the `lines` iterator (with possible errors) */
    def parseFasta():
        Iterator[ Either[ParseDenotationsError, FASTA.Value] ] =
    new Iterator[ Either[ParseDenotationsError, FASTA.Value] ] {

      /* If there is one more header, there is one more FASTA value (even if the sequence is empty) */
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

    /* This is the same as `parseFasta` dropping all erroneous FASTAs and skipping anything before the first FASTA value */
    def parseFastaDropErrors(skipCrap: Boolean = true): Iterator[FASTA.Value] = {
      if (skipCrap) cutUntil(fasta.header.is)
      parseFasta() collect { case Right(fa) => fa }
    }
  }

}
