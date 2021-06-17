package grapher.data

case class AllGraphs[GV, VV, EV](enrichedGraphs: Seq[EnrichedGraph[GV, VV, EV]],
                                 freshGraphs: Seq[FreshGraph[VV, EV]],
                                 mergedGraphs: Seq[MergedGraph[GV, VV, EV]])

object AllGraphs {
  def empty[GV, VV, EV]: AllGraphs[GV, VV, EV] =
    AllGraphs(
      enrichedGraphs = Seq.empty[EnrichedGraph[GV, VV, EV]],
      freshGraphs = Seq.empty[FreshGraph[VV, EV]],
      mergedGraphs = Seq.empty[MergedGraph[GV, VV, EV]]
    )
}