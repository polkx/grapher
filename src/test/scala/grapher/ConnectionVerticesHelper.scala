package grapher

import grapher.util.TestVertex

trait ConnectionVerticesHelper {

  def vertexId: TestVertex => Long = _.id

  def connectVertices(source: TestVertex,
                      target: TestVertex): Option[String] = {
    val condition = source.value == target.value
    val edgeValue = s"${source.id}-${target.id}"

    Option.when(condition)(edgeValue)
  }
}