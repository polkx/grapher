package grapher.util

import grapher.data.{AllGraphs, EnrichedGraph, FreshGraph, MergedGraph}
import org.scalatest.matchers.should.Matchers

object GraphCompareHelper extends Matchers {

  def compare(allGraphs: AllGraphs[String, String, String],
              expectedAllGraphs: AllGraphs[String, String, String]): Unit = {
    checkGraphsEquality(allGraphs.enrichedGraphs, expectedAllGraphs.enrichedGraphs)(sortedEnrichedGraph)
    checkGraphsEquality(allGraphs.freshGraphs, expectedAllGraphs.freshGraphs)(sortedFreshGraph)
    checkGraphsEquality(allGraphs.mergedGraphs, expectedAllGraphs.mergedGraphs)(sortedMergedGraph)
  }

  private def checkGraphsEquality[G](graphs: Seq[G],
                                     expectedGraphs: Seq[G])
                                    (sortedFunction: G => G): Unit = {
    graphs.map(sortedFunction) should contain theSameElementsAs expectedGraphs.map(sortedFunction)
  }

  private def sortedEnrichedGraph(graph: EnrichedGraph[String, String, String]
                                 ): EnrichedGraph[String, String, String] = {
    EnrichedGraph(
      value = graph.value,
      vertices = graph.vertices.sortBy(_.id),
      edges = graph.edges.sortBy(_.value)
    )
  }

  private def sortedFreshGraph(graph: FreshGraph[String, String]
                              ): FreshGraph[String, String] = {
    FreshGraph(
      vertices = graph.vertices.sortBy(_.id),
      edges = graph.edges.sortBy(_.value)
    )
  }

  private def sortedMergedGraph(graph: MergedGraph[String, String, String]
                               ): MergedGraph[String, String, String] = {
    MergedGraph(
      graphValues = graph.graphValues.sorted,
      vertices = graph.vertices.sortBy(_.id),
      edges = graph.edges.sortBy(_.value)
    )
  }
}