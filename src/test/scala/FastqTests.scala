package ohnosequences.fastarious.test

import org.scalatest.FunSuite

import ohnosequences.cosas._, types._, klists._
import ohnosequences.fastarious._, fastq._
import java.nio.file.Files
import java.io._

class FastqTests extends FunSuite {

  test("can create FASTQ values") {

    val i = "@HADFAQ!!:$#>#$@"
    val seq = "ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATCATCCACGATTTCACAACAGTGTCAACTGAACACACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCTACATATAATATATATATACCCGACCCCCTTCTACACTCCCCCCCCCCCACATGGTCATACAACT"
    val p = "+hola soy una línea perdida!"
    val qual = "#$adF!#$DAFAFa5++0-afd324safd"

    val fq = FASTQ(
      id(FastqId(i))                ::
      sequence(FastqSequence(seq))  ::
      plus(FastqPlus(p))            ::
      quality(FastqQuality(qual))   ::
      *[AnyDenotation]
    )
  }

  test("can parse fastq files") {

    val input = new File("test.fastq")

    import java.nio.file._
    import scala.collection.JavaConversions._
    // WARNING this will leak file descriptors
    val lines: Iterator[String] = Files.lines(input.toPath).iterator
    val buh = lines.parseFastq()
  }

  test("generate fastq file") {

    val i = "@HADFAQ!!:$#>#$@"
    val seq = "ATCCGTCCGTCCTGCGTCAAACGTCTGAC"
    val p = "+hola soy una línea perdida!"
    val qual = "#$adF!#$DAFAFa5++0-afd324safd"

    val fq = FASTQ(
      id(FastqId(i))                ::
      sequence(FastqSequence(seq))  ::
      plus(FastqPlus(p))            ::
      quality(FastqQuality(qual))   ::
      *[AnyDenotation]
    )

    val fastqFile =
      new File("test.fastq")

    // Files.deleteIfExists(fastqFile.toPath)

    val fastqs =
      Iterator.fill(10)(fq)

    fastqs appendTo fastqFile
  }

  test("parsing from iterator") {

    val fastaFile   = new File("test.fastq")
    val parsedFile  = new File("parsed.fastq")
    // Files.deleteIfExists(parsedFile.toPath)

    import java.nio.file._
    import scala.collection.JavaConversions._

    // WARNING this will leak file descriptors
    val lines: Iterator[String] = Files.lines(fastaFile.toPath).iterator

    lines.parseFastqDropErrors() appendTo parsedFile
  }

  test("FASTQ ops") {

    val fq = FASTQ(
      id(FastqId("MG32131.1"))                ::
      sequence(FastqSequence("AATCGGCGACT"))  ::
      plus(FastqPlus("+"))                    ::
      quality(FastqQuality("!@GFGC;=FFG"))    ::
        *[AnyDenotation]
    )

    assert { (fq drop 3).length == fq.length - 3 }

    assert { (fq.slice(3, 6).length == (6 - 3) ) }

    assert { fq.slice(3,6) == fq.drop(3).dropRight(fq.length - 6) }
  }
}
