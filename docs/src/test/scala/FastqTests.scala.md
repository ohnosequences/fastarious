
```scala
package ohnosequences.fastarious.test

import org.scalatest.FunSuite

import ohnosequences.cosas._, types._, typeSets._
import ohnosequences.fastarious._, fastq._

class FastqTests extends FunSuite {

  test("can create FASTQ values") {

    val i = "@HADFAQ!!:$#>#$@"
    val seq = "ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATCATCCACGATTTCACAACAGTGTCAACTGAACACACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCTACATATAATATATATATACCCGACCCCCTTCTACACTCCCCCCCCCCCACATGGTCATACAACT"
    val p = "+hola soy una línea perdida!"
    val qual = "#$adF!#$DAFAFa5++0-afd324safd"

    val fq = FASTQ(
      id(FastqId(i))                :~:
      sequence(FastqSequence(seq))  :~:
      plus(FastqPlus(p))            :~:
      quality(FastqQuality(qual))   :~: ∅
    )
  }
}

```




[main/scala/fasta.scala]: ../../main/scala/fasta.scala.md
[main/scala/fastq.scala]: ../../main/scala/fastq.scala.md
[main/scala/utils.scala]: ../../main/scala/utils.scala.md
[test/scala/FastaTests.scala]: FastaTests.scala.md
[test/scala/FastqTests.scala]: FastqTests.scala.md