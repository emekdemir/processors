package edu.arizona.sista.matcher.dependencies

import edu.arizona.sista.processors.Sentence
import edu.arizona.sista.matcher.Extractor
import edu.arizona.sista.matcher.dependencies.parser.Parser
import edu.arizona.sista.matcher.dependencies.parser.{Extractor => ExtractorRule}
import edu.arizona.sista.matcher.dependencies.parser.{Filter => FilterRule}

class DependencyExtractor(val pattern: String) extends Extractor {
  private var triggerFieldName = "trigger"
  private var _trigger: Option[TriggerMatcher] = None
  private var _arguments: Option[Map[String, ExtractorRule]] = None

  def trigger = getFieldValue(_trigger)
  def arguments = getFieldValue(_arguments)

  initialize(pattern)

  private def initialize(pattern: String) {
    val fieldPat = """(\w+)\s*:\s*(.+)""".r
    val it = fieldPat findAllIn pattern map {
      case fieldPat(name, value) => (name -> value)
    }
    val fields = Map(it.toSeq: _*)
    _trigger = Some(new TriggerMatcher(Parser.parseFilter(fields(triggerFieldName))))
    _arguments = Some(fields filterKeys (_ != triggerFieldName) mapValues Parser.parseExtractor)
  }

  private def getFieldValue[T](field: Option[T]) = field match {
    case None => scala.sys.error("object not initialized")
    case Some(value) => value
  }

  def findAllIn(sentence: Sentence): Seq[Map[String, Seq[Int]]] = {
    trigger findAllIn sentence flatMap (i => applyRules(sentence, i))
  }

  private def applyRules(sentence: Sentence, i: Int): Option[Map[String, Seq[Int]]] = {
    val matches = arguments.keySet flatMap { name =>
      arguments(name).findAllIn(sentence, i) match {
        case Nil => None
        case indices => Some(name -> indices)
      }
    }
    if (matches.isEmpty) None
    else Some(matches.toMap + ("trigger" -> Seq(i)))
  }
}

object DependencyExtractor {
  def apply(pattern: String) = new DependencyExtractor(pattern)
}

class TriggerMatcher(filter: FilterRule) {
  def findAllIn(sentence: Sentence): Seq[Int] =
    filter.filter(sentence, 0 until sentence.size)
}