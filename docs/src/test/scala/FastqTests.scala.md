
```scala
package ohnosequences.fastarious.test

import org.scalatest.FunSuite

import ohnosequences.cosas._, types._, klists._
import ohnosequences.fastarious._, fastq._
import better.files._

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

    val input = file"test.fastq"

    import java.nio.file._
    import scala.collection.JavaConversions._
    // WARNING this will leak file descriptors
    val lines = Files.lines(input.path).iterator
    val buh = parseFastq(lines)
  }

  test("generate fastq file") {

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

    val fastqFile = file"test.fastq"
    fastqFile.clear

    val fastqs = Iterator.fill(10000)(fq)
    fastqs appendTo fastqFile
  }

  test("parsing from iterator") {

    val fastaFile   = file"test.fastq"
    val parsedFile  = file"parsed.fastq"
    parsedFile.clear

    import java.nio.file._
    import scala.collection.JavaConversions._

    // WARNING this will leak file descriptors
    val lines   = Files.lines(fastaFile.path).iterator
    val asFastq = fastq.parseFastqDropErrors(lines)

    asFastq appendTo parsedFile
  }


}

```




[main/scala/fasta.scala]: ../../main/scala/fasta.scala.md
[main/scala/fastq.scala]: ../../main/scala/fastq.scala.md
[main/scala/ncbiHeaders.scala]: ../../main/scala/ncbiHeaders.scala.md
[main/scala/utils.scala]: ../../main/scala/utils.scala.md
[test/scala/FastaTests.scala]: FastaTests.scala.md
[test/scala/FastqTests.scala]: FastqTests.scala.md
[test/scala/NcbiHeadersTests.scala]: NcbiHeadersTests.scala.md