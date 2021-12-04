package grapher.util

import grapher.data.{AllGraphs, EnrichedGraph, FreshGraph, MergedGraph}
import org.scalatest.matchers.should.Matchers

object GraphCompareHelper extends Matchers {

  def compare(allGraphs: AllGraphs[String, TestVertex, String],
              expectedAllGraphs: AllGraphs[String, TestVertex, String]): Unit = {
    checkGraphsEquality(allGraphs.enrichedGraphs, expectedAllGraphs.enrichedGraphs)(sortedEnrichedGraph)
    checkGraphsEquality(allGraphs.freshGraphs, expectedAllGraphs.freshGraphs)(sortedFreshGraph)
    checkGraphsEquality(allGraphs.mergedGraphs, expectedAllGraphs.mergedGraphs)(sortedMergedGraph)
  }

  private def checkGraphsEquality[G](graphs: Seq[G],
                                     expectedGraphs: Seq[G])
                                    (sortedFunction: G => G): Unit = {
    graphs.map(sortedFunction) should contain theSameElementsAs expectedGraphs.map(sortedFunction)
  }

  private def sortedEnrichedGraph(graph: EnrichedGraph[String, TestVertex, String]
                                 ): EnrichedGraph[String, TestVertex, String] = {
    EnrichedGraph(
      value = graph.value,
      vertices = graph.vertices.sortBy(_.id),
      edges = graph.edges.sortBy(_.value)
    )
  }

  private def sortedFreshGraph(graph: FreshGraph[TestVertex, String]
                              ): FreshGraph[TestVertex, String] = {
    FreshGraph(
      vertices = graph.vertices.sortBy(_.id),
      edges = graph.edges.sortBy(_.value)
    )
  }

  private def sortedMergedGraph(graph: MergedGraph[String, TestVertex, String]
                               ): MergedGraph[String, TestVertex, String] = {
    MergedGraph(
      graphValues = graph.graphValues.sorted,
      vertices = graph.vertices.sortBy(_.id),
      edges = graph.edges.sortBy(_.value)
    )
  }
}