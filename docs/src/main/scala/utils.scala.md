
```scala
package ohnosequences.fastarious

case object utils {

  def removeAllSpace(s: String): String =
    (s split('\n') map { _.trim.filter(_ >= ' ') } mkString)
}

```




[test/scala/NcbiHeadersTests.scala]: ../../test/scala/NcbiHeadersTests.scala.md
[test/scala/FastqTests.scala]: ../../test/scala/FastqTests.scala.md
[test/scala/FastaTests.scala]: ../../test/scala/FastaTests.scala.md
[main/scala/fasta.scala]: fasta.scala.md
[main/scala/fastq.scala]: fastq.scala.md
[main/scala/utils.scala]: utils.scala.md
[main/scala/ncbiHeaders.scala]: ncbiHeaders.scala.md