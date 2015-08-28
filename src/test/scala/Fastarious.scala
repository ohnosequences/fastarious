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

    val uhoh = Map(
      "header" -> ">adfadfadfHOLAESTOYENCERRADOFASTAAAAA",
      "sequence" -> "ATGCGTC\nACTCTAC\n\nADTTTCGAGATCATACCCCAACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCACACACACACACGAGGGGACTTCATAQACATTATTTA"
    )

    val couldBe = FASTA parse uhoh

    println{

      couldBe match {

        case Right(zz) => (zz get header value, (zz get fasta.sequence value).lines.toList)
      }
    }

    val fastqraw = Map(
      "id" -> "@ERR475467.1 MISEQ:141:000000000-A7LTW:1:1101:18249:2044",
      "sequence"-> "TACGGAGGGGGCTAGCGTTGTTCGGAATTACTGGGCGTAAAGCGCACGTAGGCGGCTTTGTAAGTCAGAGGTGAAAGCCTGGAGCTCAACTCCAGAACTGCCTTTGAGACTGCATCGCTTGAATCCAGGAGAGGTCAGTGGAATTCCGAGTGTAGAGGTGAAATTCGTAGATATTCGGAAGAACACCAGTGGCGAAGGCGGCTGACTGGACTGGTATTGACGCTGAGGTGCGAAAGCGTGGGGAGCAAACAGG",
      "plus" -> "+",
      "quality" -> "BBBFFBBFFBBDEGGGGGGG;GHGGEGGHHHHHHHHGGGGFHDFGGGGHHHGHGGGGGHHHHHHHHFGCGDHHGHHHHHGHGFFFGFHHHHHHHHHHHHHHGHHHHFGHHHHHHHHGGHHGHFHHHHHHHGFHGHHHHHHHHHHHHHHGGEGHHHHHHHHHHF3HFHGHHGGGGGGGHHGHGHGGGGGGFGGGGGFGFFFGFFGHHGHHHHGHHHHFFFFFFGGHHGFFEFGGGGFFGGGGFFFFFFFA?A>3"
    )

    val fastqmaybe = FASTQ parse fastqraw

    println {

      fastqmaybe match {

        case Right(zz) => (zz get id value, (zz get fastq.sequence value), zz get plus value, zz get quality value )
      }
    }


  }
}
