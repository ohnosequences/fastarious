package ohnosequences.fastarious.test

import org.scalatest.FunSuite

import ohnosequences.fastarious._, fastq._
import java.nio.file._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import java.io._

class FastqTests extends FunSuite {

  def lines(jFile: File): Iterator[String] =
    Files.lines(jFile.toPath).iterator

  test("FASTQ Id") {

    assert { Id.from("@4aD#c buh boh") == Id("@4aD#c buh boh") }

    val rawId = "@HWI323 asdf:3"
    val wrongRawId = "Hola hola" // no '@'

    assert { (Id parseFrom rawId) == Some( Id("HWI323 asdf:3") ) }
    assert { (Id parseFrom wrongRawId) == None }
  }

  test("can create FASTQ values") {

    val i = "@HADFAQ!!:$#>#$@"
    val rawSeq = "ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATCATCCACGATTTCACAACAGTGTCAACTGAACACACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCTACATATAATATATATATACCCGACCCCCTTCTACACTCCCCCCCCCCCACATGGTCATACAACT"
    val rawQual = "#$adF!#$DAFAFa5++0-afd324safd"

    val fq = FASTQ.fromStringsPhred33(
      id        = i,
      sequence  = rawSeq,
      quality   = rawQual
    )

    assert { fq == None }
  }

  test("can parse fastq files") {

    val input = new File("test.fastq")

    import java.nio.file._
    import scala.collection.JavaConversions._
    // WARNING this will leak file descriptors
    val lines: Iterator[String] = Files.lines(input.toPath).iterator
    val buh = lines.parseFastqPhred33
  }

  test("generate fastq file") {

    val i       = "@HADFAQ!!:$#>#$@"
    val rawSeq  = "ATCCGTCCGTCCTGCGTCAAACGTCTGAC"
    val rawQual = "#$adF!#$DAFAFa5++0-afd324safd"

    val fq = FASTQ.fromStringsPhred33(
      id        = i,
      sequence  = rawSeq,
      quality   = rawQual
    )

    val fastqFile =
      new File("test.fastq")

    Files.deleteIfExists(fastqFile.toPath)

    val fastqs =
      fq map { Iterator.fill(1000)(_) }

    fastqs foreach { _ appendAsPhred33To fastqFile }
  }

  test("parsing from iterator") {

    val fastqFile   = new File("test.fastq")
    val parsedFile  = new File("parsed.fastq")
    // Files.deleteIfExists(parsedFile.toPath)
    // appendAsPhred33To parsedFile
  }

  test("parse and write from/to file") {

    val in  = new File("in.fastq")
    val out = new File("out.fastq")
    Files.deleteIfExists(out.toPath)

    lines(in).parseFastqPhred33DropErrors appendAsPhred33To out

    assert { lines(in).toList == lines(out).toList }
  }

  test("read and write from/to file") {

    val in  = new File("in.fastq")
    val out = new File("out.fastq")
    Files.deleteIfExists(out.toPath)

    Files.write(out.toPath, lines(in).map({ x => x: CharSequence }).toIterable.asJava , StandardOpenOption. CREATE, StandardOpenOption.WRITE)

    assert { lines(in).toList == lines(out).toList }
  }

  test("FASTQ quality") {

    val rawQual = "$adF!#$DAFAFa5++0-afd324safd"

    val optQual =
      Quality.fromPhred33(rawQual)

    optQual foreach { q =>
      assert { q.toPhred33 == rawQual }
      assert { q.value.forall { v => 0 <= v && v <= 93 } }
    }
  }

  test("FASTQ ops") {

    val fqOpt =
      FASTQ.fromStringsPhred33(
        id        = "@HADFAQ!!:$#>#$@",
        sequence  = "ATCCGTCCGTCCTGCGTCAAACGTCTGAC",
        quality   = "#$adF!#$DAFAFa5++0-afd324safd"
      )

    fqOpt foreach { fqq =>

      val fq = fqq.sequence

      assert { (fq drop 3).length == fq.length - 3 }
      assert { (fq.slice(3, 6).length == (6 - 3) ) }
      assert { fq.slice(3,6) == fq.drop(3).dropRight(fq.length - 6) }
    }
  }
}
