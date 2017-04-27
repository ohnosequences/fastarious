package ohnosequences.fastarious

import Quality._
import spire.implicits._

/*
  ## Quality

  [Phred Quality scores](https://en.wikipedia.org/wiki/Phred_quality_score), thus assumed to be positive.
*/
case class Quality private (val scores: Seq[Score]) extends AnyVal {

  final
  def toPhred33: String =
    (scores map Quality.toPhred33).mkString

  // see for example https://doi.org/10.1093/bioinformatics/btv401
  final
  def expectedErrors: BigDecimal =
    scores.foldLeft(0:BigDecimal)(
      { (acc,s) => acc + s.asPhredScore.errorProbability }
    )

  final
  def variance: BigDecimal =
    scores.foldLeft(0:BigDecimal)(
      { (acc,s) => acc + ( s.asPhredScore.errorProbability * s.asPhredScore.successProbability ) }
    )

  final
  def maxScore: Score =
    scores.max

  final
  def minScore: Score =
    scores.min
}

case object Quality {

  type Score            = Int
  type ErrorProbability = BigDecimal

  private final
  def errorProbability(n: Score): ErrorProbability =
    BigDecimal(10) fpow ( - BigDecimal(n / 10) )

  private final
  def cacheProbs: Map[Score, ErrorProbability] =
    Map( (0 to 100).map { n => (n, errorProbability(n)) } : _*)

  lazy val phred33Cache: Map[Score, ErrorProbability] =
    cacheProbs

  implicit final
  class ScoreConverters(val n: Score) extends AnyVal {

    def asPhredScore: PhredScore =
      PhredScore(n)
  }

  final case
  class PhredScore(val n: Score) extends AnyVal {

    def errorProbability: ErrorProbability =
      phred33Cache.getOrElse(n, Quality.errorProbability(n))

    def successProbability: ErrorProbability =
      1 - errorProbability
  }

  final
  def fromPhred33(raw: String): Option[Quality] = {

    @annotation.tailrec
    def rec(cs: String, acc: collection.mutable.Builder[Int,Vector[Int]], errors: Boolean): Option[Quality] =
      if(errors) { None } else {

        if(cs.isEmpty) Some( Quality(acc.result) ) else {

          cs.head.toInt match {
            case q if 33 <= q && q <= 126 => rec(cs.tail, acc += (q - 33), errors)
            case _                        => rec(cs, acc, errors = true)
          }
        }
      }

    val bldr = Vector.newBuilder[Int]; bldr.sizeHint(raw.length)

    rec(raw, bldr, false)
  }

  final
  def toPhred33(i: Score): Char =
    (i + 33).toChar
}
