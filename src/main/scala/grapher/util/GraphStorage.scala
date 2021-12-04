package grapher.util

import grapher.data.AllGraphs

trait GraphStorage[GV, VV, EV] extends GraphConnector[GV, VV, EV] {

  private[grapher] var allGraphs: AllGraphs[GV, VV, EV] = initialGraphs

  /**
   * Override when you want add starting graphs e.g. after some failure
   *
   * @return all graphs
   */
  def initialGraphs: AllGraphs[GV, VV, EV] = AllGraphs.empty

  /**
   * Handle created graph. Run this method after some interval.
   *
   * @param allGraphs graphs from local storage
   * @return graphs to which the vertices will be joined
   */
  def batchProcess(allGraphs: AllGraphs[GV, VV, EV]): AllGraphs[GV, VV, EV]

  final def process(incomingVertex: VV): Unit = {
    allGraphs = process(incomingVertex, allGraphs)
  }

  final def runBatchProcess(): Unit = {
    val allGraphsProcessed = batchProcess(allGraphs)
    allGraphs = allGraphsProcessed
  }
}