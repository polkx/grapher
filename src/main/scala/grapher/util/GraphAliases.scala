package grapher.util

import grapher.data.{EnrichedGraph, FreshGraph, MergedGraph}

trait GraphAliases[GV, VV, EV]  {
  type EnrichedGh = EnrichedGraph[GV, VV, EV]
  type FreshGh = FreshGraph[VV, EV]
  type MergedGh = MergedGraph[GV, VV, EV]
}