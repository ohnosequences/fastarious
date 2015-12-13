
```scala
package ohnosequences.fastarious

case object utils {

  def removeAllSpace(s: String): String =
    (s split('\n') map { _.trim.filter(_ >= ' ') } mkString)
}

```




[main/scala/fasta.scala]: fasta.scala.md
[main/scala/fastq.scala]: fastq.scala.md
[main/scala/utils.scala]: utils.scala.md
[test/scala/FastaTests.scala]: ../../test/scala/FastaTests.scala.md
[test/scala/FastqTests.scala]: ../../test/scala/FastqTests.scala.md