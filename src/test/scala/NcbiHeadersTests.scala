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

    println { randomIds.asFastaHeader.value.asString }
    println { randomIds.asFastqId.value.asString }
  }
}
