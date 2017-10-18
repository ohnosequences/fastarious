* #46: Added empty values for `Sequence`, `Quality`, `SequenceQuality`
* #51: Updated the build, cross-compiled with Scala 2.11/2.12
* #52: Dropped spire dependency
* #54: Dropped `ncbiHeaders` module (~~hopefully~~ it wasn't used anywhere)
* #25: Changed FASTA-related code to plain old case classes (similarly to FASTQ)
    + Changed `parseFasta` to `Iterator[FASTA]` (no `Option`)
    + Added `parseSkipCrap` (same as `parseFastaDropErrors(skipCrap = true)` before)
    + Removed `parseFastaDropErrors` (replaced with `parseFasta`)
* Dropped cosas dependency (after #25 and #54 it's not used by anything)
