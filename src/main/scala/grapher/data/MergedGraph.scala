package grapher.data

case class MergedGraph[GV, VV, EV](graphValues: Vector[GV],
                                   vertices: Vector[Vertex[VV]],
                                   edges: Seq[Edge[EV]]
                                  ) {
  def size: Int = vertices.size

  def isEmpty: Boolean = vertices.isEmpty

  def nonEmpty: Boolean = vertices.nonEmpty

  def addMergedGraph(mergedGraph: MergedGraph[GV, VV, EV]): MergedGraph[GV, VV, EV] = {
    MergedGraph(
      graphValues = graphValues ++ mergedGraph.graphValues,
      vertices = vertices ++ mergedGraph.vertices,
      edges = edges ++ mergedGraph.edges
    )
  }

  def addFreshGraph(freshGraph: FreshGraph[VV, EV]): MergedGraph[GV, VV, EV] = {
    MergedGraph(
      graphValues = graphValues,
      vertices = vertices ++ freshGraph.vertices,
      edges = edges ++ freshGraph.edges
    )
  }

  def addEnrichedGraph(enrichedGraph: EnrichedGraph[GV, VV, EV]): MergedGraph[GV, VV, EV] = {
    MergedGraph(
      graphValues = enrichedGraph.value +: graphValues,
      vertices = vertices ++ enrichedGraph.vertices,
      edges = edges ++ enrichedGraph.edges
    )
  }

  private[grapher] def addVertex(vertex: Vertex[VV]): MergedGraph[GV, VV, EV] = {
    MergedGraph(
      graphValues = graphValues,
      vertices = vertex +: vertices,
      edges = edges
    )
  }

  private[grapher] def addEdges(connectedEdges: Seq[Edge[EV]]): MergedGraph[GV, VV, EV] = {
    MergedGraph(
      graphValues = graphValues,
      vertices = vertices,
      edges = edges ++ connectedEdges
    )
  }
}

object MergedGraph {

  def empty[GV, VV, EV]: MergedGraph[GV, VV, EV] =
    MergedGraph[GV, VV, EV](Vector.empty, Vector.empty, Vector.empty)

}