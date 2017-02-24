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

    val rawId = "@HWI323 asdf:3"
    val wrongRawId = "Hola hola" // no '@'

    assert { (Id parseFrom rawId) == Some( Id("HWI323 asdf:3") ) }
    assert { (Id parseFrom wrongRawId) == None }
  }

  test("seq length ≠ qual length means none") {

    val i       = "@HADFAQ!!:$#>#$@"
    // different length for seq and qual
    val rawSeq  = "ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATCA"
    val rawQual = "#$adF!#$DAFAFa5++0-afd324safd"

    assert { FASTQ.fromStringsPhred33(id = i, sequence = rawSeq, quality = rawQual) == None }
  }

  test("parse and write from/to file is idempotent") {

    val in  = new File("in.fastq")
    val out = new File("out.fastq")
    Files.deleteIfExists(out.toPath)

    lines(in).parseFastqPhred33DropErrors appendAsPhred33To out

    assert { lines(in).toList == lines(out).toList }
  }

  test("raw read and write from/to file") {

    val in  = new File("in.fastq")
    val out = new File("out.fastq")
    Files.deleteIfExists(out.toPath)

    Files.write(out.toPath, lines(in).map({ x => x: CharSequence }).toIterable.asJava , StandardOpenOption. CREATE, StandardOpenOption.WRITE)

    assert { lines(in).toList == lines(out).toList }
  }

  test("FASTQ phred33 quality") {

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

      val fq = fqq.value

      assert { (fq drop 3).length == fq.length - 3 }
      assert { (fq.slice(3, 6).length == (6 - 3) ) }
      assert { fq.slice(3,6) == fq.drop(3).dropRight(fq.length - 6) }
    }
  }
}
