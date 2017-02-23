package ohnosequences.fastarious.test

import org.scalatest.FunSuite

import scala.collection.JavaConverters._
import ohnosequences.fastarious._, fastq._
import java.nio.file.Files
import java.io._

/*
  # Preprocessing examples
*/
case object preprocessing {

  implicit def fastqAsSequence: FASTQ => Sequence = _.value

  implicit def preprocessingOps(r: FASTQ): FastqPreprocessingOps =
    new FastqPreprocessingOps(r.value)

  implicit class FastqPreprocessingOps(val s: Sequence) extends AnyVal {

    def dropTrailingUnder(quality: Int): Sequence =
      s dropWhileQuality { _ <= quality }

    def dropWhileAverage(windowSize: Int, averageQuality: BigDecimal): Sequence = {

      def rec(acc: Sequence): Sequence =
        if(acc.isEmpty)
          acc
        else
          if( (acc takeRight windowSize).quality.average <= averageQuality )
            rec(acc dropRight windowSize)
          else
            acc

      rec(s)
    }

    def longestSuffixOver(quality: Int): Sequence =
      s takeWhileQuality { _ >= quality }

    def numberOfNs: Int =
      s countSequence { _.toUpper == 'N' }

    def countQualityOver(quality: Int): Int =
      s countQuality { _ >= quality }
  }
}

class FastqPreprocessing extends FunSuite {

  def lines(jFile: File): Iterator[String] =
    Files.lines(jFile.toPath).iterator.asScala

  def reads: Iterator[FASTQ] =
    lines(new File("in.fastq")) parseFastqPhred33DropErrors

  import preprocessing._

  test("sample preprocessing") {

    val out = new File("preprocessed.fastq")
    Files.deleteIfExists(out.toPath)

    def preprocessedReads =
      reads
        .filter { read =>
          (read.quality.average >= 30) &&
          (read.numberOfNs <= 4)
        }
        .map { read =>

          FASTQ(
            read.id,
            read
              .dropTrailingUnder(quality = 20)
              .dropWhileAverage(windowSize = 10, averageQuality = 35)
          )
        }
        .filter(_.length >= 120)

    preprocessedReads appendAsPhred33To out

    println { s"Number of raw reads: ${reads.size}" }
    println { s"Number of valid reads: ${preprocessedReads.size}" }


  }
}
