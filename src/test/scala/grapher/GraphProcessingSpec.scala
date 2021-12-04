package grapher

import grapher.GraphProcessingSpec.processVertices
import grapher.data.{AllGraphs, EnrichedGraph}
import grapher.util.{GraphCompareHelper, TestVertex}
import grapher.util.GraphConnectorSpec.{edge, graph, oneElementFreshGraph, vertex}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.wordspec.AnyWordSpec

class GraphProcessingSpec extends AnyWordSpec with BeforeAndAfterEach {

  override def afterEach(): Unit = {
    TestGraphProcessing.allGraphs = AllGraphs.empty
    TestGraphProcessing.resetCounter()
  }

  "GraphProcessing" should {

    "return empty all graphs when no vertices arrived" in {
      TestGraphProcessing.runBatchProcess()
      val connectedVertices = TestGraphProcessing.allGraphs
      val outputGraphs = AllGraphs.empty[String, TestVertex, String]
      GraphCompareHelper.compare(connectedVertices, outputGraphs)
    }

    "return all graphs with only one enriched graph with size >= 2" in {
      processVertices(1, 101, 2)
      val connectedVertices = TestGraphProcessing.allGraphs
      val outputGraphs = AllGraphs(
        enrichedGraphs = Seq(graph(1, edge(1, 101))),
        freshGraphs = Seq(oneElementFreshGraph(2)),
        mergedGraphs = Seq.empty
      )
      GraphCompareHelper.compare(connectedVertices, outputGraphs)
    }

    "return all graphs enriched" in {
      processVertices(1, 101, 2, 202)
      val connectedVertices_1 = TestGraphProcessing.allGraphs
      val outputGraphs_1 = AllGraphs(
        enrichedGraphs = Seq(graph(2, edge(1, 101)), graph(1, edge(2, 202))),
        freshGraphs = Seq.empty,
        mergedGraphs = Seq.empty
      )
      GraphCompareHelper.compare(connectedVertices_1, outputGraphs_1)

      processVertices(1001, 3)
      val connectedVertices_2 = TestGraphProcessing.allGraphs
      val updatedEnrichedGraph =
        EnrichedGraph(
          value = "1_g_updateEnrichedGraph",
          vertices = Vector(vertex(2), vertex(202)),
          edges = Seq(edge(2, 202))
        )
      val updatedMergedGraph =
        EnrichedGraph(
          value = "2_g_updateMergedGraphs",
          vertices = Vector(vertex(1), vertex(101), vertex(1001)),
          edges = Seq(edge(1, 101), edge(1, 1001), edge(101, 1001))
        )
      val outputGraphs_2 = AllGraphs(
        enrichedGraphs = Seq(updatedEnrichedGraph, updatedMergedGraph),
        freshGraphs = Seq(oneElementFreshGraph(3)),
        mergedGraphs = Seq.empty
      )
      GraphCompareHelper.compare(connectedVertices_2, outputGraphs_2)
    }

    "close graph with size > 3" in {
      processVertices(1, 101, 1001, 2)
      val connectedVertices_1 = TestGraphProcessing.allGraphs
      val outputGraphs_1 = AllGraphs(
        enrichedGraphs = Seq(graph(1, edge(1, 101), edge(1, 1001), edge(101, 1001))),
        freshGraphs = Seq(oneElementFreshGraph(2)),
        mergedGraphs = Seq.empty
      )
      GraphCompareHelper.compare(connectedVertices_1, outputGraphs_1)

      processVertices(10001)
      val connectedVertices_2 = TestGraphProcessing.allGraphs
      val outputGraphs_2: AllGraphs[String, TestVertex, String] = AllGraphs(
        enrichedGraphs = Seq.empty,
        freshGraphs = Seq(oneElementFreshGraph(2)),
        mergedGraphs = Seq.empty
      )
      GraphCompareHelper.compare(connectedVertices_2, outputGraphs_2)
    }
  }
}

object GraphProcessingSpec {
  private def processVertices(vertices: Int*): Unit = {
    vertices.foreach(id => TestGraphProcessing.process(vertex(id)))
    TestGraphProcessing.runBatchProcess()
  }
}