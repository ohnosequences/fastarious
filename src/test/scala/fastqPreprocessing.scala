package ohnosequences.fastarious.test

import org.scalatest.FunSuite

import ohnosequences.cosas._, types._, klists._
import ohnosequences.fastarious._, fastq._
import java.nio.file.Files
import java.io._

/*
  Preprocessing examples
*/
case object preprocessing {

  implicit class FastqPreprocessingOps(val s: Sequence) {

    def dropTrailingUnder(threshold: Int): Sequence =
      s dropWhileQuality { _ <= threshold }
  }
}

class FastqPreprocessing extends FunSuite {}
