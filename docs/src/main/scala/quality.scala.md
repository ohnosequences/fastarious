
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

  def :+(s: Score): Quality =
    Quality(scores :+ s)

  def +:(s: Score): Quality =
    Quality(s +: scores)

  def ++(other: Quality): Quality =
    Quality(scores ++ other.scores )

  def foldLeft[X](init: X)(op: (X,Score) => X): X =
    scores.foldLeft(init)(op)

  def reverse: Quality =
    Quality(scores.reverse)

  final
  def toPhred33: String =
    (scores map Quality.toPhred33).mkString

  final
  def errorPs: Seq[ErrorP] =
    scores map errorProbability

  final
  def successPs: Seq[Prob] =
    scores map successProbability

  // see for example https://doi.org/10.1093/bioinformatics/btv401
  final
  def expectedErrors: Num =
    scores.foldLeft(0:Num)(
      { (acc,s) => acc + errorProbability(s) }
    )

  final
  def expectedErrors_slow: Num =
    scores.foldLeft(0:Num)(
      { (acc,s) => acc + errorProbabilityImpl(s) }
    )

  final
  def variance: Num =
    scores.foldLeft(0:Num)(
      { (acc,s) => acc + ( errorProbability(s) * successProbability(s) ) }
    )

  final
  def maxScore: Score =
    scores.max

  final
  def minScore: Score =
    scores.min
}

case object Quality {

  final
  def scoreFrom(errP: ErrorP): Score =
    // ((-10) * errP.log(10)).round().intValue
    Math.round((-10:Double) * errP.log(10)).toInt


  final
  def errorProbability(n: Score): ErrorP =
    phred33ErrorPCache.getOrElse(n, errorProbabilityImpl(n))

  private final
  def errorProbabilityImpl(n: Score): ErrorP =
    // BigDecimal(10) fpow ( - (BigDecimal(n) / 10) )
    (10:Double) fpow ( - ((n:Double) / 10) )


  final
  def successProbability(n: Score): Prob =
    1 - errorProbability(n)

  lazy val phred33ErrorPCache: Map[Score, ErrorP] =
    Map( (0 to 1000).map { n => (n, errorProbabilityImpl(n)) } : _*)

  implicit final
  class ScoreConverters(val n: Score) extends AnyVal {

    def asPhredScore: PhredScore =
      PhredScore(n)
  }

  final case
  class PhredScore(val n: Score) extends AnyVal {

    def errorProbability: ErrorP =
      Quality.errorProbability(n)

    def successProbability: ErrorP =
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
[main/scala/quality.scala]: quality.scala.md
[main/scala/DNA.scala]: DNA.scala.md
[main/scala/package.scala]: package.scala.md
[main/scala/fasta.scala]: fasta.scala.md
[main/scala/fastq.scala]: fastq.scala.md
[main/scala/SequenceQuality.scala]: SequenceQuality.scala.md
[main/scala/utils.scala]: utils.scala.md
[main/scala/sequence.scala]: sequence.scala.md
[main/scala/ncbiHeaders.scala]: ncbiHeaders.scala.md