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
      header(FastaHeader(h))        ::
      sequence(FastaSequence(seq))  ::
      *[AnyDenotation]
    )

    assert {
      f.serialize[String].fold(l => l, r => FASTA parse r) === Right(f)
    }
  }

  test("line length is always 70") {

    val h = ">@HUGHA5.ADFDA#"
    val seq = "ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATCATCCACGATTTCACAACAGTGTCAACTGAACACACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCTACATATAATATATATATACCCGACCCCCTTCTACACTCCCCCCCCCCCACATGGTCATACAACT"

    val f = FASTA(
      header(FastaHeader(h))        ::
      sequence(FastaSequence(seq))  ::
      *[AnyDenotation]
    )

    val ls = f.asString
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
    val l = FASTA(
      (header   := id)           ::
      (sequence := randomLines)  ::
      *[AnyDenotation]
    )

    val fastas = Iterator.fill(10000)(l)

    fastas appendTo fastaFile
  }

  test("parsing from iterator") {

    val fastaFile   = file"test.fasta"
    val parsedFile  = file"parsed.fasta"
    parsedFile.clear

    import java.nio.file._
    import scala.collection.JavaConversions._

    // WARNING this will leak file descriptors
    val lines   = Files.lines(fastaFile.path).iterator
    val asFasta = fasta.parseFastaDropErrors(lines)

    asFasta appendTo parsedFile
  }

  test("raw parsing from iterator") {

    val fastaFile   = file"test.fasta"
    val parsedFile  = file"parsed-raw.fasta"
    parsedFile.clear

    import java.nio.file._
    import scala.collection.JavaConversions._

    // WARNING this will leak file descriptors
    val lines   = Files.lines(fastaFile.path).iterator
    val asMaps  = fasta.parseMap(lines)

    asMaps.foreach {
      map => {
        parsedFile.appendLines(s">${map("header")}")
        parsedFile.appendLines(map("sequence").grouped(70).mkString("\n"))
      }
    }
  }

  test("FASTA lines parsing") {

    val crap = Seq(
      "hola fasta! lalala",
      "oh no, no soy fasta"
    )

    val fasta1 = FASTA(
      header(FastaHeader(">1 hola")) ::
      sequence(FastaSequence("ATCACCCACTTTACATTTCACACACCCCTTTACAC")) ::
      *[AnyDenotation]
    )

    val fasta2 = FASTA(
      header(FastaHeader(">2 hola")) ::
      sequence(FastaSequence("ATATACCCACACCCCGGTCAT")) ::
      *[AnyDenotation]
    )

    val emptyFasta = FASTA(
      header(FastaHeader(">3 nothing")) ::
      sequence(FastaSequence("")) ::
      *[AnyDenotation]
    )

    assert {
      fasta.parseFastaDropErrors( (crap ++ fasta1.lines).iterator ).toList ==
        List(fasta1)
    }

    assert {
      fasta.parseFastaDropErrors( (crap ++ emptyFasta.lines ++ fasta1.lines).iterator ).toList ==
        List(emptyFasta, fasta1)
    }

    assert {
      fasta.parseFastaDropErrors( emptyFasta.lines.iterator ).toList ==
        List(emptyFasta)
    }

    assert {
      fasta.parseFastaDropErrors( (fasta1.lines ++ emptyFasta.lines).iterator ).toList ==
        List(fasta1, emptyFasta)
    }

    assert {
      fasta.parseFastaDropErrors(List(fasta1,fasta2).flatMap(_.lines).iterator).toList ==
        List(fasta1,fasta2)
    }

    assert {
      fasta.parseFastaDropErrors(List(emptyFasta,fasta2,fasta1,fasta2,fasta2,fasta1).flatMap(_.lines).iterator).toList ==
        List(emptyFasta,fasta2,fasta1,fasta2,fasta2,fasta1)
    }

    assert {
      fasta.parseFastaDropErrors(List(emptyFasta,emptyFasta,fasta2,emptyFasta,fasta1,fasta2,fasta2,fasta1,emptyFasta).flatMap(_.lines).iterator).toList ==
        List(emptyFasta,emptyFasta,fasta2,emptyFasta,fasta1,fasta2,fasta2,fasta1,emptyFasta)
    }
  }
}
