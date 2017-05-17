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
