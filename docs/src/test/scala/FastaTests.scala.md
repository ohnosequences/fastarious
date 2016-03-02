
```scala
package ohnosequences.fastarious.test

import org.scalatest.FunSuite

import ohnosequences.cosas._, types._, klists._
import ohnosequences.fastarious._, fasta._
import better.files._

class FastaTests extends FunSuite {

  test("can create FASTA values") {

    val f = FASTA(
      header(FastaHeader(">@HUGHA5.ADFDA#")) ::
      sequence(FastaSequence("ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATC")) :: *[AnyDenotation]
    )
  }

  test("can serialize and parse FASTA values") {

    val h = ">@HUGHA5.ADFDA#"
    val seq = """
    ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATC
    ATCCACGA
    TTTCACAACAGTGTCAACTGAACACACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    CCCTACATATAATATATATATACCCGA
    CCCCCTTCTACACTCCCCCCCCCCCACATGGTCATAC
    AACT
    """

    val f = FASTA(
      header(FastaHeader(h))    ::
      sequence(FastaSequence(seq)) :: *[AnyDenotation]
    )

    assert {
      f.serialize[String].fold(l => l, r => FASTA parse r) === Right(f)
    }
  }

  test("line length is always 70") {

    val h = ">@HUGHA5.ADFDA#"
    val seq = "ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATCATCCACGATTTCACAACAGTGTCAACTGAACACACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCTACATATAATATATATATACCCGACCCCCTTCTACACTCCCCCCCCCCCACATGGTCATACAACT"

    val f = FASTA(
      header(FastaHeader(h))    ::
      sequence(FastaSequence(seq)) :: *[AnyDenotation]
    )

    val ls = f.toLines
    val lsSplit = ls.split('\n')

    assert { lsSplit.filter(l => (l.length <= 70) || l.startsWith(">")) === lsSplit }
  }

  test("id and description == header value") {

    val fa = FASTA(
      header( FastaHeader("adsfa12312 que bonita secuencia") )  ::
      sequence( FastaSequence("AATATAT ATA TACACAC AAATC"))     ::
      *[AnyDenotation]
    )

    assert { s"${fa.getV(header).id}${fa.getV(header).description}" === fa.getV(header).value }
  }



  test("generate fasta file") {

    val fastaFile = file"test.fasta"
    fastaFile.clear

    val id = FastaHeader("id|12312312 una secuencia cualquiera")
    val randomLines = FastaSequence("ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATCCCCCCTTCTACACTCCCCCCCCCCCACATGGTCATTTCTACACACCCCCCCCCCCCCCCCGGGGGGGGGGGGGGGGGGGGGGGGGGGCATCCCTACATATACTTCTCGTCATACTCATACATACACCCCCCCCCCCACAGGGGTCCATACAAAGGGCTTATATCCCCACGGGTCTTTTTCACTTCATATTTTTGGGGGCCTCGCGCGCCCTTAC")

    // somewhere around 2MB
    for(i <- 1 to 10000) {

      val l = FASTA(
        (header   := id)           ::
        (sequence := randomLines)  ::
        *[AnyDenotation]
      )
      .toLines

      fastaFile.append(l)
    }
  }

  test("parsing from iterator") {

    val fastaFile   = file"test.fasta"
    val parsedFile  = file"parsed.fasta"
    parsedFile.clear

    val lines = fastaFile.lines
    val asFasta = fasta.parseFastaFromLines(lines)

    asFasta.foreach {
      case Right(fa) => parsedFile.append( fa.toLines )
      case Left(err) => ()
    }
  }

  test("raw parsing from iterator") {

    val fastaFile   = file"test.fasta"
    val parsedFile  = file"parsed-raw.fasta"
    parsedFile.clear

    val lines = fastaFile.lines
    val asMaps = fasta.parseMapFromLines(lines)

    asMaps.foreach {
      map => {
        parsedFile.appendLines(s">${map("header")}")
        parsedFile.appendLines(map("sequence").grouped(70).mkString("\n"))
      }
    }

  }
}

```




[test/scala/NcbiHeadersTests.scala]: NcbiHeadersTests.scala.md
[test/scala/FastqTests.scala]: FastqTests.scala.md
[test/scala/FastaTests.scala]: FastaTests.scala.md
[main/scala/fasta.scala]: ../../main/scala/fasta.scala.md
[main/scala/fastq.scala]: ../../main/scala/fastq.scala.md
[main/scala/utils.scala]: ../../main/scala/utils.scala.md
[main/scala/ncbiHeaders.scala]: ../../main/scala/ncbiHeaders.scala.md