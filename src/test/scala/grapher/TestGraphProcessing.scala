package grapher

import grapher.data.{AllGraphs, EnrichedGraph}

object TestGraphProcessing
  extends GraphProcessing[String, String, String]
    with ConnectionVerticesHelper {
  private val minGraphSize = 2
  private val maxGraphSize = 3
  private var freshCounter = 1

  def resetCounter(): Unit = freshCounter = 1

  override def batchProcess(allGraphs: AllGraphs[String, String, String]): AllGraphs[String, String, String] = {
    val (skippedFreshGraphs, enrichedFreshGraphs) = allGraphs.freshGraphs
      .foldLeft(Seq.empty[FreshGh], Seq.empty[EnrichedGh]) {
        case ((skippedFreshGraphsAcc, enrichedFreshGraphsAcc), freshGraph)
          if freshGraph.size >= minGraphSize && freshGraph.size <= maxGraphSize =>
          (skippedFreshGraphsAcc, enrichFreshGraph(freshGraph) +: enrichedFreshGraphsAcc)

        case ((skippedFreshGraphsAcc, enrichedFreshGraphsAcc), freshGraph) if freshGraph.size > maxGraphSize =>
          (skippedFreshGraphsAcc, enrichedFreshGraphsAcc)

        case ((skippedFreshGraphs, enrichedGraphs), freshGraph) =>
          (freshGraph +: skippedFreshGraphs, enrichedGraphs)
      }

    val updatedEnrichedGraphs = allGraphs.enrichedGraphs
      .collect { case enrichedGraph if enrichedGraph.size <= maxGraphSize => updateEnrichedGraph(enrichedGraph) }

    val updatedMergedGraphs = allGraphs.mergedGraphs
      .collect { case mergedGraph if mergedGraph.size <= maxGraphSize => updateMergedGraphs(mergedGraph) }

    AllGraphs(
      enrichedGraphs = updatedEnrichedGraphs ++ enrichedFreshGraphs ++ updatedMergedGraphs,
      freshGraphs = skippedFreshGraphs.toVector,
      mergedGraphs = Vector.empty
    )
  }

  private def updateEnrichedGraph(enrichedGraph: EnrichedGh): EnrichedGh = {
    EnrichedGraph(
      value = enrichedGraph.value + "_updateEnrichedGraph",
      vertices = enrichedGraph.vertices,
      edges = enrichedGraph.edges
    )
  }

  private def enrichFreshGraph(freshGraph: FreshGh): EnrichedGh = {
    val createdValue = s"${freshCounter}_g"
    freshCounter = freshCounter + 1
    EnrichedGraph(
      value = createdValue,
      vertices = freshGraph.vertices,
      edges = freshGraph.edges
    )
  }

  private def updateMergedGraphs(mergedGraph: MergedGh): EnrichedGh = {
    val mergedValues = mergedGraph.graphValues.mkString(",")

    EnrichedGraph(
      value = mergedValues + "_updateMergedGraphs",
      vertices = mergedGraph.vertices,
      edges = mergedGraph.edges
    )
  }
}