package org.fencepost.bloom

import org.apache.lucene.analysis._
import org.apache.lucene.document._
import org.apache.lucene.index._
import org.apache.lucene.search._
import org.apache.lucene.store.RAMDirectory
import org.apache.lucene.util._

import scala.io._

// Filter implementation based on Lucene.  Mainly used here for comparison to
// Bloom filter results.
class LuceneFilter(vals:Stream[String]) extends Filter[String] {

  val directory = new RAMDirectory()
  val config = new IndexWriterConfig(Version.LUCENE_31,new WhitespaceAnalyzer(Version.LUCENE_31))
  val writer = new IndexWriter(directory,config)

  // We're already dealing with a sequence of lines only so there's no need for
  // further analysis or tokenizing.'
  val doc = new Document()
  for (aval <- vals) { doc.add(new Field("content",aval,Field.Store.YES,Field.Index.NOT_ANALYZED)) }
  writer.addDocument(doc)
  writer.commit()
  writer.close()

  val searcher = new IndexSearcher(directory)

  override def contains(arg:String):Boolean = {

    val topdocs = searcher.search(new TermQuery(new Term("content",arg)),1)
    topdocs.totalHits > 0
  }
}
