
```scala
package ohnosequences.fastarious

import Quality._
import spire.implicits._
```


## Quality

[Phred Quality scores](https://en.wikipedia.org/wiki/Phred_quality_score), thus assumed to be positive.


```scala
case class Quality(val scores: Seq[Score]) extends AnyVal {

  def isEmpty: Boolean =
    scores.isEmpty

  def length: Int =
    scores.length

  def at(index: Int): Option[Score] =
    if( index < 0 || (length - 1) < index) None else Some( scores(index) )

  def headOption: Option[Score] =
    if(isEmpty) None else Some { scores.head }

  def tailOption: Option[Quality] =
    if(isEmpty) None else Some { drop(1) }

  def drop(n: Int): Quality =
    Quality( scores drop n )

  def dropRight(n: Int): Quality =
    Quality( scores dropRight n )

  def slice(from: Int, until: Int): Quality =
    Quality( scores.slice(from, until) )

  def take(n: Int): Quality =
    Quality( scores take n )

  def takeRight(n: Int): Quality =
    Quality( scores takeRight n )

  def ++(other: Quality): Quality =
    Quality(scores ++ other.scores )

  def reverse: Quality =
    Quality(scores.reverse)

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
    BigDecimal(10) fpow ( - (BigDecimal(n) / 10) )

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

```




[test/scala/DNA.scala]: ../../test/scala/DNA.scala.md
[test/scala/NcbiHeadersTests.scala]: ../../test/scala/NcbiHeadersTests.scala.md
[test/scala/FastqTests.scala]: ../../test/scala/FastqTests.scala.md
[test/scala/FastaTests.scala]: ../../test/scala/FastaTests.scala.md
[test/scala/QualityScores.scala]: ../../test/scala/QualityScores.scala.md
[main/scala/DNAQ.scala]: DNAQ.scala.md
[main/scala/qualityScores.scala]: qualityScores.scala.md
[main/scala/DNA.scala]: DNA.scala.md
[main/scala/fasta.scala]: fasta.scala.md
[main/scala/fastq.scala]: fastq.scala.md
[main/scala/SequenceQuality.scala]: SequenceQuality.scala.md
[main/scala/utils.scala]: utils.scala.md
[main/scala/sequence.scala]: sequence.scala.md
[main/scala/ncbiHeaders.scala]: ncbiHeaders.scala.md