package ohnosequences.fastarious

/*
  ## NCBI sequence IDs

  See these for a reference:

  - https://en.wikipedia.org/wiki/FASTA_format#Sequence_identifiers
  - https://www.ncbi.nlm.nih.gov/toolkit/doc/book/ch_demo/?rendertype=table#ch_demo.T5
  - https://www.ncbi.nlm.nih.gov/genbank/sequenceids/
  - https://ncbi.github.io/cxx-toolkit/pages/ch_demo#ch_demo.id1_fetch.html_ref_fasta
*/
case object ncbiHeaders {

  case class Accession(val acc: String, val locus: String) {
    override def toString: String = Seq(
      acc,
      locus
    ).mkString("|")
  }

  trait AnyNcbiID {
    type Value
    val  value: Value
  }

  trait NcbiID[T] extends AnyNcbiID { _: Product =>
    type Value = T

    override def toString: String = Seq(
      productPrefix.toLowerCase,
      utils.removeAllSpace(value.toString)
    ).mkString("|")
  }

  case class ID(val value: String)    extends NcbiID[String] {
    override def toString: String = utils.removeAllSpace(value)
  }

  case class LCL(val value: String)   extends NcbiID[String]

  case class GI(val value: Int)       extends NcbiID[Int]

  case class GB(val value: Accession) extends NcbiID[Accession]

  /* this field corresponds to the name that is normally at the end, with spaces and all that */
  case class Name(val value: String)  extends NcbiID[String] {
    override def toString: String = s" ${value.trim}"
  }

  // TODO add the rest of ids here

  case class NcbiHeader(
    id   : ID,
    lcl  : Option[LCL],
    gb   : Option[GB],
    gi   : Option[GI],
    name : Option[Name]
  ) {

    // NOTE: the order is important
    override def toString: String = Seq[Option[AnyNcbiID]](
      Some(id),
      lcl,
      gb,
      gi,
      name
    ).flatten.mkString("|")

    // TODO
    // def asFastaHeader: fasta.Header = ???
    // def asFastqId: fastq.Id = ???
  }
}
