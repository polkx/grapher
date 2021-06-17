package grapher

import grapher.data.Vertex

trait ConnectionVerticesHelper {
  def connectVertices(source: Vertex[String],
                      target: Vertex[String]): Option[String] = {
    val condition = source.value == target.value
    val edgeValue = s"${source.id}-${target.id}"

    Option.when(condition)(edgeValue)
  }
}