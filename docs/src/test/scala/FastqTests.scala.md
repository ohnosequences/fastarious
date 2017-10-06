
```scala
package ohnosequences.fastarious.test

import org.scalatest.FunSuite

import ohnosequences.fastarious._, fastq._
import java.nio.file._
import scala.collection.JavaConverters._
import java.io._

class FastqTests extends FunSuite {

  def lines(jFile: File): Iterator[String] =
    Files.lines(jFile.toPath).iterator.asScala

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

    assert { FASTQ.fromStringsPhred33(id = i, letters = rawSeq, quality = rawQual) == None }
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
      assert { q.scores.forall { v => 0 <= v && v <= 93 } }
    }
  }

  test("FASTQ examples") {

    val fqOpt =
      FASTQ.fromStringsPhred33(
        id      = "@HADFAQ!!:$#>#$@",
        letters = "ATCCGTCCGTCCTGCGTCAAACGTCTGAC",
        quality = "#$adF!#$DAFAFa5++0-afd324safd"
      )

    fqOpt foreach { fqq =>

      val fq = fqq.sequence

      assert { (fq drop 3).length == fq.length - 3 }
      assert { (fq.slice(3, 6).length == (6 - 3) ) }
      assert { fq.slice(3,6) == fq.drop(3).dropRight(fq.length - 6) }
    }
  }
}

```




[test/scala/DNA.scala]: DNA.scala.md
[test/scala/NcbiHeadersTests.scala]: NcbiHeadersTests.scala.md
[test/scala/FastqTests.scala]: FastqTests.scala.md
[test/scala/FastaTests.scala]: FastaTests.scala.md
[test/scala/QualityScores.scala]: QualityScores.scala.md
[main/scala/DNAQ.scala]: ../../main/scala/DNAQ.scala.md
[main/scala/quality.scala]: ../../main/scala/quality.scala.md
[main/scala/DNA.scala]: ../../main/scala/DNA.scala.md
[main/scala/package.scala]: ../../main/scala/package.scala.md
[main/scala/fasta.scala]: ../../main/scala/fasta.scala.md
[main/scala/fastq.scala]: ../../main/scala/fastq.scala.md
[main/scala/SequenceQuality.scala]: ../../main/scala/SequenceQuality.scala.md
[main/scala/utils.scala]: ../../main/scala/utils.scala.md
[main/scala/sequence.scala]: ../../main/scala/sequence.scala.md
[main/scala/ncbiHeaders.scala]: ../../main/scala/ncbiHeaders.scala.md