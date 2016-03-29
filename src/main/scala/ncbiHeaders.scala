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
  case object id    extends Type[String]("") with NcbiID
  case object lcl   extends Type[String]("lcl") with NcbiID
  case object gi    extends Type[Option[Int]]("gi") with NcbiID
  case object gb    extends Type[Option[accession]]("gb") with NcbiID
  /* this field corresponds to the name that is normally at the end, with spaces and all that */
  case object name  extends Type[String]("") with NcbiID

  case class accession(val acc: String, val locus: String) {

    def asString: String = s"${acc}|${locus}"
  }

  // NOTE the order is important in this record
  case object ncbiHeader extends RecordType(id :×: lcl :×: gb :×: gi :×: name :×: |[AnyType]) {

    // we do need better records :(
    type RealRaw =
      (id.type := id.Raw)     ::
      (lcl.type := lcl.Raw)   ::
      (gb.type := gb.Raw)     ::
      (gi.type := gi.Raw)     ::
      (name.type := name.Raw) ::
      *[AnyDenotation]

    implicit def buh(v: ncbiHeader.type := ncbiHeader.RealRaw): ncbiHeaderOps = ncbiHeaderOps(v.value)
  }

  case object toHeader extends DepFn1[Any,String] {

    // first one, no need to add a '|'
    implicit val atFirst: AnyApp1At[toHeader.type, id.type := String] { type Y = String } =
      toHeader at { iv => utils.removeAllSpace(iv.value) }

    // for all the others, add '|' first
    implicit def default[I <: NcbiID { type Raw = String }]: AnyApp1At[toHeader.type, I := String] { type Y = String } =
      toHeader at {
        iv: I := String =>
          if(iv.value.nonEmpty)
            s"|${iv.tpe.label}|${utils.removeAllSpace(iv.value)}"
          else ""
      }
    implicit val atGB: AnyApp1At[toHeader.type, gb.type := gb.Raw] { type Y = String } =
      toHeader at { u => s"|${u.tpe.label}|${u.value.fold("")(_.asString)}" }

    implicit val atGI: AnyApp1At[toHeader.type, gi.type := gi.Raw] { type Y = String } =
      toHeader at { u => s"|${u.tpe.label}|${u.value.fold("")(_.toString)}" }

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
