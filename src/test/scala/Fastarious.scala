package ohnosequences.fastarious.test

import org.scalatest.FunSuite

import ohnosequences.cosas._, types._, typeSets._
import ohnosequences.fastarious._, fastq._, fasta._

class FastariousTest extends FunSuite {

  test("do something") {

    val v = FASTQ(
      fastq.id("adsfadfad") :~:
      fastq.sequence("ATGATAGAGATAGATGATTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTATCTCTTTTCCAGAGTGGGGGTATGGATGGGTATAAAAAAGGGGGTAGGGATGAGATAAAATGGGTGAACCCTACCCTACTCTCTTTTTTTTTTTTTTTTTTTTTTTT") :~:
      fastq.plus("+")   :~:
      fastq.quality("aq23#@!$!DAF$A%Q#$!ASVCSGH^@#$#@!$~@@E @Q#$Q#$Q") :~: ∅
    )

    val zzz = v.toFASTA

    println( (zzz get fasta.sequence).value.lines.toString )

    println( v.toLines.mkString("\n") )

    println(zzz.toLines.mkString("\n"))
  }
}
