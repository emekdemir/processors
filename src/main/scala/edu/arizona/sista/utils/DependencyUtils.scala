package edu.arizona.sista.utils

import edu.arizona.sista.processors.Sentence
import edu.arizona.sista.struct.{DirectedGraph, Interval}

/**
 * Utility functions for use with directed (dependency) graphs
 * User: danebell
 * Date: 2/23/15
 */
object DependencyUtils {


  val defaultPolicy: (Seq[Int]) => Int = _.last
  /**
   * Given an Interval, finds the minimal span covering all of the Interval's nodes' children (recursively).
   *
   * @param span Interval of nodes
   * @param graph a directed graph containing the nodes in span
   * @return the minimal Interval that contains all the nodes that are children of span's nodes
   */
  def subgraph(span: Interval, graph: DirectedGraph[String]): Interval = {

    val heads = if (span.size < 2) Seq(span.start) else findHeads(span, graph)

    def followTrail (i: Int): Seq[Int] = {
      val deps = graph.getOutgoingEdges(i)
      deps match {
        case empty if deps.isEmpty => Seq(i)
        case _ => (for (child <- deps) yield followTrail(child._1)).flatten
      }
    }

    val outgoing = (for (h <- heads) yield followTrail(h)).flatten.distinct

    // outgoing may only have a single index
    if (outgoing.length > 1) new Interval(outgoing.min, outgoing.max) else new Interval(outgoing.min, outgoing.min + 1)
  }

  /**
   * Finds the highest node (i.e. closest to a root) in an Interval of a directed graph. If there are multiple nodes of
   * the same rank, chooseWhich adjudicates which single node is returned.
   *
   * @param span an Interval of nodes
   * @param graph a directed graph containing the nodes in span
   * @param chooseWhich a function deciding which of multiple heads is returned; the rightmost head selected by default
   * @return the single node which is closest to the root among those in span
   */
  def findHead(span: Interval, graph: DirectedGraph[String], chooseWhich:(Seq[Int]) => Int = defaultPolicy): Int = {
    chooseWhich(findHeads(span, graph))
  }

  /**
   * Finds the highest node (i.e. closest to a root) in an Interval of a directed graph. If there are multiple nodes of
   * the same rank, all are returned.
   *
   * @param span an Interval of nodes
   * @param graph a directed graph containing the nodes in span
   * @return the single node which is closest to the root among those in span
   */
  def findHeads(span: Interval, graph: DirectedGraph[String]): Seq[Int] = {

    def getIncoming(i: Int) = graph.incomingEdges.lift(i).getOrElse(Array.empty).map(_._1)

    def followTrail (i: Int, heads:Seq[Int]): Seq[Int] = {

      // dependents
      val incoming = getIncoming(i)

      incoming match {
        case valid if valid.isEmpty | valid.contains(i) => Seq(i)
        case found if found.min < span.start | found.max > (span.end - 1) => followTrailOut(found.head, heads ++ Seq(i), i)
        case _ => followTrail(incoming.head, heads ++ Seq(i))
      }
    }

    def followTrailOut (i: Int, heads:Seq[Int], highest: Int): Seq[Int] = {

      val incoming = getIncoming(i)

      incoming match {
        case valid if valid.isEmpty | valid.contains(i) => Seq(highest)
        case outgoing if outgoing.min < span.start | outgoing.max > (span.end - 1) => followTrailOut (incoming.head, heads ++ Seq(i), highest)
        case _ => followTrail (incoming.head, heads ++ Seq(i))
      }
    }

    val heads = for (i <- span.start until span.end) yield followTrail(i, Nil)

    heads.flatten.distinct.sorted
  }

  /**
   * Find the single highest node in an interval of a dependency graph, ignoring punctuation, coordinations, and
   * prepositions.
   * @param span the interval within which to search
   * @param sent the Sentence within which to look
   * @param chooseWhich the function to adjudicate which is highest when there's a tie
   * @return Option containing the highest node index, or None if no such node is found
   */
  def findHeadStrict(span: Interval, sent: Sentence, chooseWhich:(Seq[Int]) => Int = defaultPolicy): Option[Int] = {
    val hds = findHeadsStrict(span, sent)
    hds match{
      case valid if hds.nonEmpty => Some(chooseWhich(hds.get))
      case _ => None
    }
  }

  /**
   * Find the highest nodes in an interval of a dependency graph, ignoring puncutation, coordinations, and prepositions.
   * Allows multiple node indices to be "highest" in the case of a tie.
   * @param span the interval within which to search
   * @param sent the Sentence within which to look
   * @return Option containing a sequence of highest node indices, or None if no such node is found
   */
  def findHeadsStrict(span: Interval, sent: Sentence): Option[Seq[Int]] = {

    if (sent.dependencies.isEmpty) return None

    val stopTags = "(.|,|\\(|\\)|:|''|``|#|$|CC|TO|IN)"

    def getIncoming(i: Int) = sent.dependencies.get.incomingEdges.lift(i).getOrElse(Array.empty).map(_._1)

    def followTrail (i: Int, heads:Seq[Int]): Seq[Int] = {

      // dependents
      val incoming = getIncoming(i)

      incoming match {
        case valid if valid.isEmpty | valid.contains(i) =>  Seq(i)
        case found if found.min < span.start | found.max > (span.end - 1) => followTrailOut(found.head, heads ++ Seq(i), i)
        case _ => followTrail(incoming.head, heads ++ Seq(i))
      }
    }

    def followTrailOut (i: Int, heads:Seq[Int], highest: Int): Seq[Int] = {

      val incoming = getIncoming(i)

      incoming match {
        case valid if valid.isEmpty | valid.contains(i) => Seq(highest)
        case outgoing if outgoing.min < span.start | outgoing.max > (span.end - 1) => followTrailOut (incoming.head, heads ++ Seq(i), highest)
        case _ => followTrail (incoming.head, heads ++ Seq(i))
      }
    }

    val heads = for (i <- span.start until span.end) yield followTrail(i, Nil)

    val filtered = heads.flatten.distinct.filter(x => !(sent.tags.get(x) matches stopTags)).sorted
    filtered match {
      case nonempty if filtered.length > 0 => Some(filtered)
      case _ => None
    }
  }


  /**
   * Finds the highest node (i.e. closest to a root) in an Interval of a directed graph, ignoring the graph outside the
   * Interval. If there are multiple nodes of the same rank, chooseWhich adjudicates which single node is returned.
   * <p>
   * Crucially, any node that has an incoming edge from outside the Interval is considered a head. This is efficient if
   * you know your span has a single head that is also within the span.
   *
   * @param span an Interval of nodes
   * @param graph a directed graph containing the nodes in span
   * @param chooseWhich a function deciding which of multiple heads is returned; the rightmost head selected by default
   * @return the single node which is closest to the root among those in span
   */
  def findHeadLocal(span: Interval, graph: DirectedGraph[String], chooseWhich:(Seq[Int]) => Int = defaultPolicy): Int = {
    chooseWhich(findHeadsLocal(span, graph))
  }

  /**
   * Finds the highest node (i.e. closest to a root) in an Interval of a directed graph. If there are multiple nodes of
   * the same rank, all are returned.
   * <p>
   * Crucially, any node that has an incoming edge from outside the Interval is considered a head. This is efficient if
   * you know your span has a single head that is also within the span.
   *
   * @param span an Interval of nodes
   * @param graph a directed graph containing the nodes in span
   * @return the single node which is closest to the root among those in span
   */
  def findHeadsLocal(span: Interval, graph: DirectedGraph[String]): Seq[Int] = {

    def followTrail (i: Int, heads:Seq[Int]): Seq[Int] = {

      // dependents
      val incoming = graph.getIncomingEdges(i).map(_._1)

      incoming match {
        case valid if valid.isEmpty | valid.contains(i) =>  Seq(i)
        case found if heads.contains(i) | found.min < span.start | found.max > (span.end - 1) => Seq(i)
        case _ => followTrail(incoming.head, heads ++ Seq(i))
      }
    }

    val heads = for (i <- span.start until span.end) yield followTrail(i, Nil)

    heads.flatten.distinct.toSeq.sorted
  }
}