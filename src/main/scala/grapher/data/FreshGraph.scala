package grapher.data

case class FreshGraph[VV, EV](vertices: Vector[Vertex[VV]],
                              edges: Seq[Edge[EV]]) {

  def size: Int = vertices.size

  def isEmpty: Boolean = vertices.isEmpty

  def nonEmpty: Boolean = vertices.nonEmpty

  def addFreshGraph(freshGraph: FreshGraph[VV, EV]): FreshGraph[VV, EV] = {
    FreshGraph(
      vertices = vertices ++ freshGraph.vertices,
      edges = edges ++ freshGraph.edges
    )
  }

  private[grapher] def addVertex(vertex: Vertex[VV]): FreshGraph[VV, EV] = {
    FreshGraph(
      vertices = vertex +: vertices,
      edges = edges
    )
  }

  private[grapher] def addEdges(connectedEdges: Seq[Edge[EV]]): FreshGraph[VV, EV] = {
    FreshGraph(
      vertices = vertices,
      edges = edges ++ connectedEdges
    )
  }
}

object FreshGraph {
  def empty[VV, EV]: FreshGraph[VV, EV] =
    FreshGraph(Vector.empty[Vertex[VV]], Seq.empty[Edge[EV]])

  def apply[VV, EV](vertex: Vertex[VV]): FreshGraph[VV, EV] =
    FreshGraph(Vector(vertex), Seq.empty[Edge[EV]])
}