
```scala
package ohnosequences.fastarious

case object DNA {

  implicit final
  class DNASyntax(val seq: Sequence) {

    def asDNA: DNAops =
      DNAops(seq)
  }

  case class DNAops(val seq: Sequence) {

    def complement: Sequence =
      Sequence(seq.letters.map(complementaryChar))
  }

  def complementaryChar(c: Char): Char =
    c match {
      case 'A'  => 'T'
      case 'T'  => 'A'
      case 'C'  => 'G'
      case 'G'  => 'C'
      case 'a'  => 't'
      case 't'  => 'a'
      case 'c'  => 'g'
      case 'g'  => 'c'
      case  x   =>  x
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