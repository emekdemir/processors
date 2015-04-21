package edu.arizona.sista.odin

import scala.util.hashing.MurmurHash3._
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin.impl.StringMatcher

trait Mention extends Equals with Ordered[Mention] {
  def labels: Seq[String]
  def tokenInterval: Interval
  def sentence: Int
  def document: Document
  def keep: Boolean
  val arguments: Map[String, Seq[Mention]]

  // default label
  def label: String = labels.head

  // name of matching rule
  def foundBy: String

  def start: Int = tokenInterval.start
  def end: Int = tokenInterval.end

  // character offsets
  def startOffset: Int = document.sentences(sentence).startOffsets(start)
  def endOffset: Int = document.sentences(sentence).endOffsets(end - 1)

  def matches(label: String): Boolean = labels contains label
  def matches(matcher: StringMatcher): Boolean = labels exists matcher.matches

  // returns all tokens in mention
  def words: Seq[String] = document.sentences(sentence).words.slice(start, end)
  def tags: Seq[String] = document.sentences(sentence).tags.get.slice(start, end)
  def text: String = words.mkString(" ")

  override def canEqual(a: Any) = a.isInstanceOf[Mention]

  override def equals(that: Any): Boolean = that match {
    case that: Mention => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  def compare(that: Mention): Int = {
    require(this.document == that.document)

    (this.sentence - that.sentence,
      this.tokenInterval.start - that.tokenInterval.start,
      this.tokenInterval.end - that.tokenInterval.end) match {
      case (0,0,x) => x
      case (0,x,_) => x
      case (x,_,_) => x
    }
  }

  def precedes(that: Mention): Boolean = {
    this.compare(that) match {
      case c if c < 0 => true
      case _ => false
    }
  }

  override def hashCode: Int = {
    val h0 = symmetricSeed
    val h1 = mix(h0, labels.hashCode)
    val h2 = mix(h1, tokenInterval.hashCode)
    val h3 = mix(h2, sentence.hashCode)
    val h4 = mix(h3, document.hashCode)
    val h5 = mixLast(h4, arguments.hashCode)
    finalizeHash(h5, 5)
  }
}

class TextBoundMention(
    val labels: Seq[String],
    val tokenInterval: Interval,
    val sentence: Int,
    val document: Document,
    val keep: Boolean,
    val foundBy: String
) extends Mention {

  def this(
    label: String,
    tokenInterval: Interval,
    sentence: Int,
    document: Document,
    keep: Boolean,
    foundBy: String
  ) = this(Seq(label), tokenInterval, sentence, document, keep, foundBy)

  // TextBoundMentions don't have arguments
  val arguments: Map[String, Seq[Mention]] = Map.empty

  override def canEqual(a: Any) = a.isInstanceOf[TextBoundMention]

}

class EventMention(
    val labels: Seq[String],
    val trigger: TextBoundMention,
    val arguments: Map[String, Seq[Mention]],
    val sentence: Int,
    val document: Document,
    val keep: Boolean,
    val foundBy: String
) extends Mention {

  def this(
    label: String,
    trigger: TextBoundMention,
    arguments: Map[String, Seq[Mention]],
    sentence: Int,
    document: Document,
    keep: Boolean,
    foundBy: String
  ) = this(Seq(label), trigger, arguments, sentence, document, keep, foundBy)

  // token interval that contains trigger and all matched arguments
  override def tokenInterval: Interval = {
    val allStarts = trigger.start +: arguments.values.flatMap(_.map(_.start)).toSeq
    val allEnds = trigger.end +: arguments.values.flatMap(_.map(_.end)).toSeq
    Interval(allStarts.min, allEnds.max)
  }

  override def canEqual(a: Any) = a.isInstanceOf[EventMention]

}

class RelationMention(
    val labels: Seq[String],
    val arguments: Map[String, Seq[Mention]],
    val sentence: Int,
    val document: Document,
    val keep: Boolean,
    val foundBy: String
) extends Mention {

  require(arguments.values.flatten.nonEmpty, "RelationMentions need arguments")

  def this(
    label: String,
    arguments: Map[String, Seq[Mention]],
    sentence: Int,
    document: Document,
    keep: Boolean,
    foundBy: String
  ) = this(Seq(label), arguments, sentence, document, keep, foundBy)

  // token interval that contains all matched arguments
  override def tokenInterval: Interval = {
    val allStarts = arguments.values.flatMap(_.map(_.start)).toSeq
    val allEnds = arguments.values.flatMap(_.map(_.end)).toSeq
    Interval(allStarts.min, allEnds.max)
  }
}
