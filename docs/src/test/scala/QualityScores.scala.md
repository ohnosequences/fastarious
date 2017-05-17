
```scala
package ohnosequences.fastarious.test

import org.scalatest.FunSuite
import ohnosequences.fastarious._, fasta._

class QualityScores extends FunSuite {

  test("error probability calculations") {

    import Quality._

    val score1 = 30
    val score2 = 31

    assert { score1.asPhredScore.errorProbability != score2.asPhredScore.errorProbability }
    assert { score1.asPhredScore.errorProbability === BigDecimal(0.001) }

    assert { score2.asPhredScore.errorProbability + score2.asPhredScore.successProbability === 1 }

    val quals =
      Quality( Seq.fill(10)(31) )

    assert { quals.expectedErrors === 31.asPhredScore.errorProbability * 10 }
    assert { quals.maxScore === quals.minScore }
  }
}

```




[test/scala/DNA.scala]: DNA.scala.md
[test/scala/NcbiHeadersTests.scala]: NcbiHeadersTests.scala.md
[test/scala/FastqTests.scala]: FastqTests.scala.md
[test/scala/FastaTests.scala]: FastaTests.scala.md
[test/scala/QualityScores.scala]: QualityScores.scala.md
[main/scala/DNAQ.scala]: ../../main/scala/DNAQ.scala.md
[main/scala/qualityScores.scala]: ../../main/scala/qualityScores.scala.md
[main/scala/DNA.scala]: ../../main/scala/DNA.scala.md
[main/scala/fasta.scala]: ../../main/scala/fasta.scala.md
[main/scala/fastq.scala]: ../../main/scala/fastq.scala.md
[main/scala/SequenceQuality.scala]: ../../main/scala/SequenceQuality.scala.md
[main/scala/utils.scala]: ../../main/scala/utils.scala.md
[main/scala/sequence.scala]: ../../main/scala/sequence.scala.md
[main/scala/ncbiHeaders.scala]: ../../main/scala/ncbiHeaders.scala.md