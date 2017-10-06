
```scala
package ohnosequences.fastarious

import ohnosequences.cosas._, types._, records._, fns._, klists._
```


## NCBI sequence IDs

See these for a reference:

- https://en.wikipedia.org/wiki/FASTA_format#Sequence_identifiers
- https://www.ncbi.nlm.nih.gov/toolkit/doc/book/ch_demo/?rendertype=table#ch_demo.T5
- https://www.ncbi.nlm.nih.gov/genbank/sequenceids/
- https://ncbi.github.io/cxx-toolkit/pages/ch_demo#ch_demo.id1_fetch.html_ref_fasta


```scala
case object ncbiHeaders {

  trait NcbiID extends AnyType
  // TODO add the rest of ids here
  case object id    extends Type[String]("")              with NcbiID
  case object lcl   extends Type[Option[String]]("lcl")   with NcbiID
  case object gi    extends Type[Option[Int]]("gi")       with NcbiID
  case object gb    extends Type[Option[accession]]("gb") with NcbiID
```

this field corresponds to the name that is normally at the end, with spaces and all that

```scala
  case object name  extends Type[Option[String]]("")              with NcbiID

  case class accession(val acc: String, val locus: String) {

    def asString: String = s"${acc}|${locus}"
  }

  // NOTE the order is important in this record
  case object ncbiHeader extends RecordType(
    id    :×:
    lcl   :×:
    gb    :×:
    gi    :×:
    name  :×:
    |[AnyType]
  ){

    // we do need better records :(
    type RealRaw =
      (id.type    := id.Raw)   ::
      (lcl.type   := lcl.Raw)  ::
      (gb.type    := gb.Raw)   ::
      (gi.type    := gi.Raw)   ::
      (name.type  := name.Raw) ::
      *[AnyDenotation]

    implicit def ncbiHeaderOps(v: ncbiHeader.type := ncbiHeader.RealRaw): NcbiHeaderOps = NcbiHeaderOps(v.value)
  }

  case object toHeader extends DepFn1[Any,String] {

    // first one, no need to add a '|'
    implicit val atFirst: AnyApp1At[toHeader.type, id.type := String] { type Y = String } =
      toHeader at { iv => utils.removeAllSpace(iv.value) }

    // for all the others, add '|' first
    implicit def default[I <: NcbiID { type Raw = Option[String] }]: AnyApp1At[toHeader.type, I := Option[String]] { type Y = String } =
      toHeader at {
        iv => iv.value.fold("")(v => s"|${iv.tpe.label}|${utils.removeAllSpace(v)}")
      }
    implicit val atGB: AnyApp1At[toHeader.type, gb.type := gb.Raw] { type Y = String } =
      toHeader at { u => u.value.fold("")(v => s"|${u.tpe.label}|${v.asString}") }

    implicit val atGI: AnyApp1At[toHeader.type, gi.type := gi.Raw] { type Y = String } =
      toHeader at { u => u.value.fold("")( v => s"|${u.tpe.label}|${v.toString}") }

    // but for 'name', which just needs a space
    implicit val atName: AnyApp1At[toHeader.type, name.type := Option[String]] { type Y = String } =
      toHeader at { u => s"${u.value.fold("")(v => s" ${v.trim}")}" }
  }

  case class NcbiHeaderOps(v: ncbiHeader.RealRaw) extends AnyVal {

    // def asFastaHeader: fasta.header.type := fasta.FastaHeader =
    //   fasta.header(fasta.FastaHeader( v.map(toHeader).asList.mkString("") ))
    //
    // def asFastqId: fastq.id.type := fastq.FastqId =
    //   fastq.id(fastq.FastqId( v.map(toHeader).asList.mkString("") ))
  }
}

```




[test/scala/DNA.scala]: ../../test/scala/DNA.scala.md
[test/scala/NcbiHeadersTests.scala]: ../../test/scala/NcbiHeadersTests.scala.md
[test/scala/FastqTests.scala]: ../../test/scala/FastqTests.scala.md
[test/scala/FastaTests.scala]: ../../test/scala/FastaTests.scala.md
[test/scala/QualityScores.scala]: ../../test/scala/QualityScores.scala.md
[main/scala/DNAQ.scala]: DNAQ.scala.md
[main/scala/quality.scala]: quality.scala.md
[main/scala/DNA.scala]: DNA.scala.md
[main/scala/package.scala]: package.scala.md
[main/scala/fasta.scala]: fasta.scala.md
[main/scala/fastq.scala]: fastq.scala.md
[main/scala/SequenceQuality.scala]: SequenceQuality.scala.md
[main/scala/utils.scala]: utils.scala.md
[main/scala/sequence.scala]: sequence.scala.md
[main/scala/ncbiHeaders.scala]: ncbiHeaders.scala.md