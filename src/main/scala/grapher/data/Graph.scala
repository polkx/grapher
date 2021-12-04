package grapher.data

trait Graph[VV, EV] {
  def vertices: Vector[VV]

  def edges: Seq[Edge[EV]]
}