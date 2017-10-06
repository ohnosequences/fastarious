
```scala
package ohnosequences.fastarious

import DNA._

case object DNAQ {

  implicit final
  class DNAQSyntax(val seq: SequenceQuality) {

    def asDNAQ: DNAQOps =
      DNAQOps(seq)
  }

  case class DNAQOps(val seq: SequenceQuality) {

    def complement: SequenceQuality =
      SequenceQuality( seq.sequence.asDNA.complement, seq.quality )
  }
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