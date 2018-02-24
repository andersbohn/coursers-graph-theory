package scalagraph

import scalax.collection.GraphTraversal.{DepthFirst, Parameters, Successors}

object EulerianCycles extends App {

  import scalax.collection.Graph
  import scalax.collection.edge.Implicits._
  import scalax.collection.edge.LDiEdge
  import scalax.collection.io.dot._
  import implicits._

  val nodes = List("a", "b", "c", "d")
  val edges = List(
    ("a" ~+> "b") ("x"),
    ("b" ~+> "c") ("x"),
    ("c" ~+> "e") ("x"),
    ("e" ~+> "a") ("x"),
    ("a" ~+> "d") ("x"),
    ("d" ~+> "c") ("x"),
    ("c" ~+> "a") ("x"),
    ("b" ~+> "f") ("x")
  )

  val graph = Graph[String, LDiEdge](edges :_*)

  val isStronglyConnected = graph.nodes.headOption map { head =>
    head.innerNodeTraverser(
      Parameters(kind = DepthFirst, direction = Successors)).size == nodes.size
  } getOrElse true


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

  val dot2 = graph.toDot(root2, edgeTransformer)
  println(s"dot2 - $isStronglyConnected\n" + dot2)
}
