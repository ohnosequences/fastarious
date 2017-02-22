package ohnosequences.fastarious.test

import org.scalatest.FunSuite

import ohnosequences.cosas._, types._, klists._
import ohnosequences.fastarious._, fastq._
import java.nio.file.Files
import java.io._

/*
  # Preprocessing examples
*/
case object preprocessing {

  implicit class FastqPreprocessingOps(val s: Sequence) extends AnyVal {

    def dropTrailingUnder(threshold: Int): Sequence =
      s dropWhileQuality { _ <= threshold }

    def dropWhileAverage(windowSize: Int, average: BigDecimal): Sequence = {

      def rec(acc: Sequence): Sequence =
        if(acc.isEmpty)
          acc
        else
          if( (acc takeRight windowSize).quality.average <= average )
            rec(acc dropRight windowSize)
          else
            acc

      rec(s)
    }

    def longestSuffixOver(quality: Int): Sequence =
      s takeWhileQuality { _ >= quality }

    def numberOfNs: Int =
      s countSequence { _.toUpper == 'N' }

    def countQualityOver(threshold: Int): Int =
      s countQuality { _ >= threshold }
  }
}

class FastqPreprocessing extends FunSuite {}
