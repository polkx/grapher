package grapher.data

case class EnrichedGraph[GV, VV, EV](value: GV,
                                     vertices: Vector[VV],
                                     edges: Seq[Edge[EV]]
                                    ) extends Graph[VV, EV] {

  def size: Int = vertices.size

  def isEmpty: Boolean = vertices.isEmpty

  def nonEmpty: Boolean = vertices.nonEmpty
}