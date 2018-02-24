package scalagraph

import scalax.collection.{Graph, GraphEdge}
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

object GuariniSolver extends App {

  val nodes = (0 to 7).permutations.toSet.map { lst: IndexedSeq[Int] =>
    val node = (0 to 7).map { ix =>
      if (ix == lst(0)) 'W'
      else if (ix == lst(1)) 'W'
      else if (ix == lst(2)) 'B'
      else if (ix == lst(3)) 'B'
      else '*'
    }.mkString
    node
  }
  val moves = IndexedSeq(
    List(4, 6),
    List(5, 7),
    List(3, 6),
    List(2, 7),
    List(0, 5),
    List(1, 4),
    List(0, 2),
    List(1, 3)
  )

  val edges = nodes.flatMap { configuration =>
    (0 to 7).flatMap { moveIx =>
      moves(moveIx).flatMap { newPos =>
        (configuration.charAt(moveIx), configuration.charAt(newPos)) match {
          case p@('W' | 'B', '*') =>
            val arr = configuration.toCharArray
            arr(moveIx) = '*'
            arr(newPos) = p._1
            val newNode = configuration ~ arr.mkString
            Some(newNode)
          case _ =>
            None
        }
      }
    }
  }

  println(s"nodes ${nodes.size}")
  println(s"edges ${edges.size}")

  val graph = Graph.from(nodes, edges)

  println(s"graph nodes ${graph.nodes.size}")
  println(s"graph edges ${graph.edges.size}")
  println(s"graph.isConnected ${graph.isConnected}")

  val components = for (component <- graph.componentTraverser())
    yield {
      println(s"component ${component.nodes.size}")
      component
    }

  def checkPair(cg: Graph[String, GraphEdge.UnDiEdge], p1: String, p2: String): Unit = {
    println(s""" ${cg.size} - - $p1 ${cg.find(p1)} - $p2:  ${cg.find(p2)} """)
  }


  components.foreach { c =>
    val cg = c.toGraph
    checkPair(cg, "W*B**W*B", "W*W**B*B")
    checkPair(cg, "B*B**W*W", "W*W**B*B")
    checkPair(cg, "W*B**B*W", "W*W**B*B")
  }

  val node1 = graph.find("W*W**B*B").get
  val node2 = graph.find("B*B**W*W").get
  val sp = (node1 shortestPathTo node2).get
  println(sp.nodes.toSeq.mkString("\n"))
}
