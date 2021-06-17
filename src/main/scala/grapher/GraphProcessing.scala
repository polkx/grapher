package grapher

import grapher.util.GraphStorage

/**
 * Main trait used to creating graphs.
 * Operations is run in one thread.
 * Operations are not thread-safe.
 *
 * @tparam GraphValue  value describing graphs
 * @tparam VertexValue value describing vertex
 * @tparam EdgeValue   type of created edges
 */
trait GraphProcessing[GraphValue, VertexValue, EdgeValue] extends GraphStorage[GraphValue, VertexValue, EdgeValue]