package scalagraph

import scalax.collection.Graph
import scalax.collection.GraphPredef._

object TopologicalSort extends App {

  import scalax.collection.edge.LDiEdge._
  import scalax.collection.Graph
  import scalax.collection.edge.LDiEdge, scalax.collection.edge.Implicits._
  import scalax.collection.io.dot._
  import implicits._

  val nodes = List("a", "b", "c", "d")
  val edges = List(
    ("a" ~+> "b") ("ab"),
    ("b" ~+> "c") ("bc"),
    ("b" ~+> "d") ("bd"),
    ("d" ~+> "c") ("dc"),
    ("a" ~+> "d") ("ad")
  )

  val graph = Graph[String, LDiEdge](edges :_*)

  val sorted = if (graph.isDirected && graph.isAcyclic) {
    println("top sorted ")
    graph.topologicalSort
    graph
  } else {
    println("not dag ")
    graph
  }

  val root2 = DotRootGraph(directed = true, id = Some("Basic Graph"))

  def edgeTransformer(innerEdge: Graph[String, LDiEdge]#EdgeT):
  Option[(DotGraph, DotEdgeStmt)] = innerEdge.edge match {
    case LDiEdge(source, target, label) => label match {
      case label: String =>
        Some((root2,
          DotEdgeStmt(source.toString,
            target.toString,
            if (label.nonEmpty) List(DotAttr("label", label.toString))
            else Nil)))
    }
  }

  val dot2 = sorted.toDot(root2, edgeTransformer)
  println("dot2\n" + dot2)
}
