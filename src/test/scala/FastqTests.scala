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
      plus(p)                       :~:
      quality(FastqQuality(qual))   :~: ∅
    )
  }
}
