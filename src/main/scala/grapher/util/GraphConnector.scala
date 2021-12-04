package grapher.util

import grapher.data._

trait GraphConnector[GV, VV, EV] extends GraphAliases[GV, VV, EV] {

  private val emptyMergedGraph = MergedGraph.empty[GV, VV, EV]
  private val emptyFreshGraph = FreshGraph.empty[VV, EV]

  /**
   * Get id from vertex
   *
   * @return vertex id
   */
  def vertexId: VV => Long

  /**
   * How edges will be joined
   *
   * @param source         vertex from local storage
   * @param incomingVertex incoming vertex to connect
   * @return created edge. If empty vertices are not connected.
   */
  def connectVertices(source: VV, incomingVertex: VV): Option[EV]

  /**
   * Override when you want create edges based on enriched graph
   *
   * @param incomingVertex incoming vertex to connect
   * @param enrichedGraph  enriched graph from local graph storage
   * @return connected edges to enriched graph
   */
  def createEdges(incomingVertex: VV,
                  enrichedGraph: EnrichedGh): Seq[Edge[EV]] = {
    createEdges(incomingVertex, enrichedGraph.vertices)
  }

  /**
   * Override when you want create edges based on merged graph
   *
   * @param incomingVertex incoming vertex to connect
   * @param mergedGraph    merged graph from local graph storage
   * @return connected edges to merged graph
   */
  def createEdges(incomingVertex: VV,
                  mergedGraph: MergedGh): Seq[Edge[EV]] = {
    createEdges(incomingVertex, mergedGraph.vertices)
  }

  private[grapher] def process(incomingVertex: VV,
                             allGraphs: AllGraphs[GV, VV, EV]
                            ): AllGraphs[GV, VV, EV] = {
    val (notConnectedMergedGraphs, connectedMergedGraph) =
      connectToMergedGraphs(incomingVertex, allGraphs.mergedGraphs)
    val (notConnectedSingleGraphs, connectedSingleGraph) =
      connectToEnrichedGraphs(incomingVertex, allGraphs.enrichedGraphs)
    val (notConnectedFreshGraphs, connectedFreshGraph) =
      connectFreshGraphs(incomingVertex, allGraphs.freshGraphs)

    if (connectedMergedGraph.nonEmpty || connectedSingleGraph.nonEmpty) {
      val newMergedGraph =
        connectedMergedGraph
          .addMergedGraph(connectedSingleGraph)
          .addFreshGraph(connectedFreshGraph)
          .addVertex(incomingVertex)

      AllGraphs(
        enrichedGraphs = notConnectedSingleGraphs,
        freshGraphs = notConnectedFreshGraphs,
        mergedGraphs = newMergedGraph +: notConnectedMergedGraphs
      )
    } else if (connectedFreshGraph.nonEmpty) {
      val connectedFreshGraphWithIncomingVertex =
        connectedFreshGraph.addVertex(incomingVertex)

      AllGraphs(
        enrichedGraphs = notConnectedSingleGraphs,
        freshGraphs = connectedFreshGraphWithIncomingVertex +: notConnectedFreshGraphs,
        mergedGraphs = notConnectedMergedGraphs
      )
    } else { // if the vertex has not been connected
      val oneElementFreshGraph = FreshGraph[VV, EV](incomingVertex)
      AllGraphs(
        enrichedGraphs = notConnectedSingleGraphs,
        freshGraphs = oneElementFreshGraph +: notConnectedFreshGraphs,
        mergedGraphs = notConnectedMergedGraphs
      )
    }
  }

  private def connectToMergedGraphs(incomingVertex: VV,
                                    connectedGraphs: Seq[MergedGh]
                                   ): (Seq[MergedGh], MergedGh) = {
    connectedGraphs.foldLeft(Seq.empty[MergedGh], emptyMergedGraph) {
      case ((notConnectedMergedGraphs, connectedMergedGraph), mergedGraph) =>
        val connectedEdges = createEdges(incomingVertex, mergedGraph)
        if (connectedEdges.nonEmpty) {
          val updatedMergedGraph =
            connectedMergedGraph
              .addMergedGraph(mergedGraph)
              .addEdges(connectedEdges)

          (notConnectedMergedGraphs, updatedMergedGraph)
        } else {
          (mergedGraph +: notConnectedMergedGraphs, connectedMergedGraph)
        }
    }
  }

  private def connectToEnrichedGraphs(incomingVertex: VV,
                                      graphs: Seq[EnrichedGh]
                                     ): (Seq[EnrichedGh], MergedGh) = {
    graphs.foldLeft(Seq.empty[EnrichedGh], emptyMergedGraph) {
      case ((notConnectedGraphs, connectedGraphs), enrichedGraph) =>
        val createdEdges = createEdges(incomingVertex, enrichedGraph)
        if (createdEdges.nonEmpty) {
          val updatedMergedGraph =
            connectedGraphs
              .addEnrichedGraph(enrichedGraph)
              .addEdges(createdEdges)

          (notConnectedGraphs, updatedMergedGraph)
        } else {
          (enrichedGraph +: notConnectedGraphs, connectedGraphs)
        }
    }
  }

  private def connectFreshGraphs(incomingVertex: VV,
                                 freshGraphs: Seq[FreshGh]
                                ): (Seq[FreshGh], FreshGh) = {
    freshGraphs.foldLeft(Seq.empty[FreshGh], emptyFreshGraph) {
      case ((notConnectedGraphs, connectedGraph), freshGraph) =>
        val createdEdges = createEdges(incomingVertex, freshGraph.vertices)
        if (createdEdges.nonEmpty) {
          val updatedFreshGraph =
            connectedGraph
              .addFreshGraph(freshGraph)
              .addEdges(createdEdges)

          (notConnectedGraphs, updatedFreshGraph)
        } else {
          (freshGraph +: notConnectedGraphs, connectedGraph)
        }
    }
  }

  private def createEdges(incomingVertex: VV,
                          vertexInGraph: Vector[VV]): Seq[Edge[EV]] = {
    vertexInGraph.foldLeft(Seq.empty[Edge[EV]]) {
      case (edgesFromIncomingVertex, vertex) =>
        val edgeValueOpt = connectVertices(vertex, incomingVertex)

        if (edgeValueOpt.isDefined) {
          val edge = Edge(
            source = vertexId(vertex),
            target = vertexId(incomingVertex),
            value = edgeValueOpt.get
          )

          edge +: edgesFromIncomingVertex
        } else {
          edgesFromIncomingVertex
        }
    }
  }
}