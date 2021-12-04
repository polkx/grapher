package grapher.util

import grapher.data._
import grapher.util.GraphConnectorSpec._
import org.scalatest.wordspec.AnyWordSpec

class GraphConnectorSpec extends AnyWordSpec {
  "GraphConnector process" should {

    "return allGraphs with one fresh Graph" in {
      val inputGraphs = AllGraphs(
        enrichedGraphs = Vector.empty[EnrichedGraph[String, TestVertex, String]],
        freshGraphs = Vector.empty[FreshGraph[TestVertex, String]],
        mergedGraphs = Vector.empty[MergedGraph[String, TestVertex, String]]
      )

      val outputGraphs = AllGraphs(
        enrichedGraphs = Vector.empty[EnrichedGraph[String, TestVertex, String]],
        freshGraphs = Vector(oneElementFreshGraph(101)),
        mergedGraphs = Vector.empty[MergedGraph[String, TestVertex, String]]
      )

      val processedInput = TestGraphConnector.process(vertex101, inputGraphs)
      GraphCompareHelper.compare(outputGraphs, processedInput)
    }

    "connect vertex to enrichedGraph" in {
      val inputGraphs = AllGraphs(
        enrichedGraphs = Vector(oneElementGraph(1, 1)),
        freshGraphs = notConnectedFreshGraph,
        mergedGraphs = notConnectedMergedGraph
      )

      val outputGraphs = AllGraphs(
        enrichedGraphs = Vector.empty,
        freshGraphs = notConnectedFreshGraph,
        mergedGraphs = notConnectedMergedGraph :+ MergedGraph(
          graphValues = Vector("1_g"),
          vertices = Vector(vertex101, vertex(1)),
          edges = Vector(edge1)
        )
      )

      val processedInput = TestGraphConnector.process(vertex101, inputGraphs)
      GraphCompareHelper.compare(outputGraphs, processedInput)
    }

    "connect two enriched graphs together" in {
      val inputGraphs = AllGraphs(
        enrichedGraphs = Vector(oneElementGraph(1, 1), oneElementGraph(2, 1001)),
        freshGraphs = notConnectedFreshGraph,
        mergedGraphs = notConnectedMergedGraph
      )

      val outputGraphs = AllGraphs(
        enrichedGraphs = Vector.empty,
        freshGraphs = notConnectedFreshGraph,
        mergedGraphs = notConnectedMergedGraph :+ MergedGraph(
          graphValues = Vector("1_g", "2_g"),
          vertices = Vector(vertex(1), vertex101, vertex(1001)),
          edges = Vector(edge1, edge1001)
        )
      )

      val processedInput = TestGraphConnector.process(vertex101, inputGraphs)
      GraphCompareHelper.compare(outputGraphs, processedInput)
    }

    "connect vertex to freshGraph" in {
      val inputGraphs = AllGraphs(
        enrichedGraphs = notConnectedEnrichedGraph,
        freshGraphs = Vector(oneElementFreshGraph(1)),
        mergedGraphs = notConnectedMergedGraph
      )

      val outputGraphs = AllGraphs(
        enrichedGraphs = notConnectedEnrichedGraph,
        freshGraphs = Vector(freshGraph(edge1)),
        mergedGraphs = notConnectedMergedGraph
      )

      val processedInput = TestGraphConnector.process(vertex101, inputGraphs)
      GraphCompareHelper.compare(outputGraphs, processedInput)
    }

    "connect two fresh graphs together" in {
      val inputGraphs = AllGraphs(
        enrichedGraphs = notConnectedEnrichedGraph,
        freshGraphs = Vector(oneElementFreshGraph(1), oneElementFreshGraph(1001)),
        mergedGraphs = notConnectedMergedGraph
      )

      val outputGraphs = AllGraphs(
        enrichedGraphs = notConnectedEnrichedGraph,
        freshGraphs = Vector(freshGraph(edge1, edge1001)),
        mergedGraphs = notConnectedMergedGraph
      )

      val processedInput = TestGraphConnector.process(vertex101, inputGraphs)
      GraphCompareHelper.compare(outputGraphs, processedInput)
    }

    "connect vertex to mergedGraph" in {
      val inputGraphs = AllGraphs(
        enrichedGraphs = notConnectedEnrichedGraph,
        freshGraphs = notConnectedFreshGraph,
        mergedGraphs = Vector(
          MergedGraph(
            graphValues = Vector("1_g"),
            vertices = Vector(vertex(1), vertex(2)),
            edges = Vector(edge(1, 2))
          )
        )
      )

      val outputGraphs = AllGraphs(
        enrichedGraphs = notConnectedEnrichedGraph,
        freshGraphs = notConnectedFreshGraph,
        mergedGraphs = Vector(
          MergedGraph(
            graphValues = Vector("1_g"),
            vertices = Vector(vertex(1), vertex101, vertex(2)),
            edges = Vector(edge1, edge(1, 2))
          )
        )
      )

      val processedInput = TestGraphConnector.process(vertex101, inputGraphs)
      GraphCompareHelper.compare(outputGraphs, processedInput)
    }

    "connect two merged graphs together" in {
      val inputGraphs = AllGraphs(
        enrichedGraphs = notConnectedEnrichedGraph,
        freshGraphs = notConnectedFreshGraph,
        mergedGraphs = Vector(
          MergedGraph(
            graphValues = Vector("1_g"),
            vertices = Vector(vertex(2), vertex(1)),
            edges = Vector(edge(1, 2))
          ),
          MergedGraph(
            graphValues = Vector("2_g"),
            vertices = Vector(vertex(3), vertex(1001)),
            edges = Vector(edge(1001, 3))
          )
        )
      )

      val outputGraphs = AllGraphs(
        enrichedGraphs = notConnectedEnrichedGraph,
        freshGraphs = notConnectedFreshGraph,
        mergedGraphs = Vector(
          MergedGraph(
            graphValues = Vector("1_g", "2_g"),
            vertices = Vector(vertex(2), vertex(1), vertex(3), vertex(1001), vertex101),
            edges = Vector(edge(1, 2), edge(1001, 3), edge1, edge1001)
          )
        )
      )

      val processedInput = TestGraphConnector.process(vertex101, inputGraphs)
      GraphCompareHelper.compare(outputGraphs, processedInput)
    }

    "connect enrichedGraph and freshGraph" in {
      val inputGraphs = AllGraphs(
        enrichedGraphs = Vector(graph(1, edge(1001, 2))),
        freshGraphs = Vector(freshGraph(edge(1, 3))),
        mergedGraphs = notConnectedMergedGraph
      )

      val outputGraphs = AllGraphs(
        enrichedGraphs = Vector.empty,
        freshGraphs = Vector.empty,
        mergedGraphs = notConnectedMergedGraph :+ MergedGraph(
          graphValues = Vector("1_g"),
          vertices = Vector(vertex(1001), vertex(1), vertex101, vertex(2), vertex(3)),
          edges = Vector(edge1, edge1001, edge(1001, 2), edge(1, 3))
        )
      )

      val processedInput = TestGraphConnector.process(vertex101, inputGraphs)
      GraphCompareHelper.compare(outputGraphs, processedInput)
    }

    "connect enrichedGraph and mergedGraph" in {
      val inputGraphs = AllGraphs(
        enrichedGraphs = Vector(graph(1, edge(1001, 2))),
        freshGraphs = notConnectedFreshGraph,
        mergedGraphs = Vector(
          MergedGraph(
            graphValues = Vector("2_g"),
            vertices = Vector(vertex(1), vertex(3)),
            edges = Vector(edge(1, 3))
          )
        )
      )

      val outputGraphs = AllGraphs(
        enrichedGraphs = Vector.empty,
        freshGraphs = notConnectedFreshGraph,
        mergedGraphs = Vector(
          MergedGraph(
            graphValues = Vector("1_g", "2_g"),
            vertices = Vector(vertex(1001), vertex(1), vertex101, vertex(2), vertex(3)),
            edges = Vector(edge1, edge1001, edge(1001, 2), edge(1, 3))
          )
        )
      )

      val processedInput = TestGraphConnector.process(vertex101, inputGraphs)
      GraphCompareHelper.compare(outputGraphs, processedInput)
    }

    "connect freshGraph and mergedGraph" in {
      val inputGraphs = AllGraphs(
        enrichedGraphs = notConnectedEnrichedGraph,
        freshGraphs = Vector(freshGraph(edge(1001, 2))),
        mergedGraphs = Vector(
          MergedGraph(
            graphValues = Vector("1_g"),
            vertices = Vector(vertex(1), vertex(3)),
            edges = Vector(edge(1, 3))
          )
        )
      )

      val outputGraphs = AllGraphs(
        enrichedGraphs = notConnectedEnrichedGraph,
        freshGraphs = Vector.empty,
        mergedGraphs = Vector(
          MergedGraph(
            graphValues = Vector("1_g"),
            vertices = Vector(vertex(1001), vertex(1), vertex101, vertex(2), vertex(3)),
            edges = Vector(edge1, edge1001, edge(1001, 2), edge(1, 3))
          )
        )
      )

      val processedInput = TestGraphConnector.process(vertex101, inputGraphs)
      GraphCompareHelper.compare(outputGraphs, processedInput)
    }

    "connect all graph" in {
      val inputGraphs = AllGraphs(
        enrichedGraphs = Vector(graph(1, edge(1, 2))),
        freshGraphs = Vector(freshGraph(edge(1001, 3))),
        mergedGraphs = Vector(
          MergedGraph(
            graphValues = Vector("2_g"),
            vertices = Vector(vertex(10001), vertex(4)),
            edges = Vector(edge(10001, 4))
          )
        )
      )

      val outputGraphs = AllGraphs(
        enrichedGraphs = Vector.empty,
        freshGraphs = Vector.empty,
        mergedGraphs = Vector(
          MergedGraph(
            graphValues = Vector("1_g", "2_g"),
            vertices = Vector(vertex(1001), vertex(10001), vertex(1), vertex101, vertex(2), vertex(3), vertex(4)),
            edges = Vector(edge1, edge1001, edge10001, edge(1001, 3), edge(1, 2), edge(10001, 4))
          )
        )
      )

      val processedInput = TestGraphConnector.process(vertex101, inputGraphs)
      GraphCompareHelper.compare(outputGraphs, processedInput)
    }
  }
}

object GraphConnectorSpec {
  private val vertex101 = vertex(101)
  private val edge1: Edge[String] = edge(1, 101)
  private val edge1001: Edge[String] = edge(1001, 101)
  private val edge10001: Edge[String] = edge(10001, 101)

  private val notConnectedEnrichedGraph = Vector(oneElementGraph(10, 20))
  private val notConnectedFreshGraph = Vector(oneElementFreshGraph(30))
  private val notConnectedMergedGraph = Vector(
    MergedGraph(
      graphValues = Vector("20_g"),
      vertices = Vector(vertex(40), vertex(50)),
      edges = Vector(edge(40, 50))
    )
  )

  private[grapher] def vertex(id: Int): TestVertex = {
    TestVertex(id, s"${id.toString.takeRight(2).toInt}_v")
  }

  private[grapher] def edge(source: Int, target: Int): Edge[String] = {
    Edge(source, target, s"$source-$target")
  }

  private[grapher] def oneElementGraph(id: Int, vertexId: Int): EnrichedGraph[String, TestVertex, String] = {
    EnrichedGraph(s"${id}_g", Vector(vertex(vertexId)), Seq.empty)
  }

  private[grapher] def graph(id: Int, edges: Edge[String]*): EnrichedGraph[String, TestVertex, String] = {
    val vertices = verticesFromEdges(edges)
    EnrichedGraph(s"${id}_g", vertices, edges.toList)
  }

  private[grapher] def oneElementFreshGraph(vertexId: Int): FreshGraph[TestVertex, String] = {
    FreshGraph(Vector(vertex(vertexId)), Seq.empty)
  }

  private[grapher] def freshGraph(edges: Edge[String]*): FreshGraph[TestVertex, String] = {
    val vertices = verticesFromEdges(edges)
    FreshGraph(vertices, edges.toList)
  }

  private def verticesFromEdges(edges: Seq[Edge[String]]): Vector[TestVertex] = {
    edges
      .flatMap(edge => Vector(edge.source, edge.target))
      .map(vertexId => vertex(vertexId.toInt))
      .distinct
      .toVector
  }
}