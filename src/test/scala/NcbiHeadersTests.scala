package ohnosequences.fastarious.test

import org.scalatest.FunSuite

import ohnosequences.cosas._, types._, klists._, records._
import ohnosequences.fastarious._, fasta._, ncbiHeaders._

class NcbiHeadersTests extends FunSuite {

  val randomIds = ncbiHeader(
    gb(accession("A3CFTC4.4", "X4CC8HG"))           ::
    gi(21312324)                                    ::
    name("A really interesting sequence hola hola") ::
    *[AnyDenotation]
  )

  test("can construct a header string from an ncbi record value") {

    val a1 = randomIds.asFastaHeader.value.asString
    val a2 = randomIds.asFastqId.value.asString
  }

  test("ncbi ids example use") {

    // use the ids before and the sequence, Note the ugly seq etc
    val seq = """
    ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATC
    ATCCACGA
    TTTCACAACAGTGTCAACTGACCCCCCCCCCCCCCCCCCCCCCCCCCC
    CCCTACATATAATATATATATACCCGA
    CCCCCTTCTACACTCCCCCCCCCCCACATGGTCATAC
    ACACACCCCCCCCCCCCCC
    AACT
    ACACACCCCCCCC
    TTTTCTCTCCCCCTTTTTTTT
    """

    val fa = FASTA(
      randomIds.asFastaHeader   ::
      sequence(FastaSequence(seq)) ::
      *[AnyDenotation]
    )
  }
}
