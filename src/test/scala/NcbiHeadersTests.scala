package ohnosequences.fastarious.test

import org.scalatest.FunSuite
import ohnosequences.fastarious._, ncbiHeaders._

class NcbiHeadersTests extends FunSuite {

  val randomIds = NcbiHeader(
    ID("1AC3438D"),
    Some(LCL("db.rna16s")),
    Some(GB(Accession("A3CFTC4.4", "X4CC8HG"))),
    Some(GI(21312324)),
    Some(Name("A really interesting sequence hola hola"))
  )

  val someMissingFields = NcbiHeader(
    ID("1AC3438D"),
    None,
    Some(GB(Accession("A3CFTC4.4", "X4CC8HG"))),
    None,
    Some(Name("cosas de la vida"))
  )

  ignore("can construct a header string from an ncbi record value") {

    // val a1 = randomIds.asFastaHeader.value.toString
    // assert { a1 == ">1AC3438D|lcl|db.rna16s|gb|A3CFTC4.4|X4CC8HG|gi|21312324 A really interesting sequence hola hola" }
    //
    // assert {
    //   someMissingFields.asFastaHeader == (fasta.header := FastaHeader("1AC3438D|gb|A3CFTC4.4|X4CC8HG cosas de la vida"))
    // }
  }

  ignore("ncbi ids example use") {

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

    // val fa = FASTA(
    //   randomIds.asFastaHeader       ::
    //   sequence(FastaSequence(seq))  ::
    //   *[AnyDenotation]
    // )
  }
}
