package ohnosequences.fastarious

import ohnosequences.cosas._, types._, records._, fns._, klists._

/*
  ## NCBI sequence IDs

  See these for a reference:

  - https://en.wikipedia.org/wiki/FASTA_format#Sequence_identifiers
  - https://www.ncbi.nlm.nih.gov/toolkit/doc/book/ch_demo/?rendertype=table#ch_demo.T5
  - https://www.ncbi.nlm.nih.gov/genbank/sequenceids/
  - https://ncbi.github.io/cxx-toolkit/pages/ch_demo#ch_demo.id1_fetch.html_ref_fasta
*/
case object ncbiHeaders {

  trait NcbiID extends AnyType
  // TODO add the rest of ids here
  case object gi    extends Type[Int]("gi") with NcbiID
  case object gb    extends Type[accession]("gb") with NcbiID
  /* this field corresponds to the name that is normally at the end, with spaces and all that */
  case object name  extends Type[String]("") with NcbiID

  class accession(val acc: String, val locus: String) {

    override def toString: String = s"${acc}|${locus}"
  }

  case object accession {

    def apply(acc: String, locus: String): accession = new accession(acc,locus)
  }

  // NOTE the order is important in this record
  case object ncbiHeader extends RecordType(gb :×: gi :×: name :×: |[AnyType]) {

    // we do need better records :(
    type RealRaw = (gb.type := gb.Raw) :: (gi.type := gi.Raw) :: (name.type := name.Raw) :: *[AnyDenotation]

    implicit def buh(v: ncbiHeader.type := ncbiHeader.RealRaw): ncbiHeaderOps = ncbiHeaderOps(v.value)
  }

  case object toHeader extends DepFn1[Any,String] {

    // first one, no need to add a '|'
    implicit val atFirst: AnyApp1At[toHeader.type, gb.type := accession] { type Y = String } =
      toHeader at { iv => s"${iv.tpe.label}|${utils.removeAllSpace(iv.value.toString)}" }

    // for all the others, add '|' first
    implicit def default[I <: NcbiID, V <: I#Raw]: AnyApp1At[toHeader.type, I := V] { type Y = String } =
      toHeader at { iv: I := V => s"|${iv.tpe.label}|${utils.removeAllSpace(iv.value.toString)}" }

    // but for 'name', which just needs a space I don't know why
    implicit val atName: AnyApp1At[toHeader.type, name.type := String] { type Y = String } =
      toHeader at { u => s" ${u.value}" }
  }

  case class ncbiHeaderOps(v: ncbiHeader.RealRaw) extends AnyVal {

    def asFastaHeader: fasta.header.type := fasta.FastaHeader =
      fasta.header(fasta.FastaHeader( v.map(toHeader).asList.mkString("") ))

    def asFastqId: fastq.id.type := fastq.FastqId =
      fastq.id(fastq.FastqId( v.map(toHeader).asList.mkString("") ))
  }
}
