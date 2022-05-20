package org.benedetto.mincostmaxflow

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class MinCostFlowTest extends AnyFlatSpec {

  def mcf = new MinCostFlow()

  // Example from the paper "Primal method for minimal cost flow"
  private val nodes = List(1,2,3,4,5)
  private val p = Problem(
    nodes,
    caps = Map((1,2) -> 10, (1,3) -> 8, (3,2) -> 5, (3,4) -> 10, (2,5) -> 2, (4,5) -> 4),
    costs = Map((1,2) -> 4, (1,3) -> 1, (3,2) -> 2, (3,4) -> 3, (2,5) -> 1, (4,5) -> 2)
  )
  private val f = Map((1,3) -> 4, (1,2) -> 0, (3,2) -> 0, (2,5) -> 0, (3,4) -> 4, (4,5) -> 4)
  private val g_of_x = Problem(
    nodes,
    caps = Map((1,2) -> 10, (1,3) -> 4, (3,1) -> 4, (3,2) -> 5, (3,4) -> 6, (4,3) -> 4, (2,5) -> 2, (5,4) -> 4),
    costs = Map((1,2) -> 4, (1,3) -> 1, (3,1) -> -1, (3,2) -> 2, (3,4) -> 3, (4,3) -> -3, (2,5) -> 1, (5,4) -> -2)
  )

  "the mincostflow" should "compute G(X) correctly" in {
    mcf.computeG(p, f) should be (g_of_x)
  }

  "the mincostflow" should "carry out a pivot step correctly" in {
    val replaced1 = mcf.replace(g_of_x.costs, 1, nodes)
    val replaced2 = mcf.replace(replaced1, 2, nodes)
    val replaced3 = mcf.replace(replaced2, 3, nodes)
    val replaced4 = mcf.replace(replaced3, 4, nodes)
    replaced4((5,5)) should be (-2)
  }

  "the mincostflow" should "determine the presence of negative cycles correctly" in {
    mcf.checkForNegativeCycle(g_of_x) should be (Some(5))
    val p2 = Problem(
      nodes = List(1,2,3,4,5),
      caps = Map((1,2) -> 10, (1,3) -> 4, (3,1) -> 4, (3,2) -> 5, (3,4) -> 6, (4,3) -> 4, (2,5) -> 2, (5,4) -> 4),
      costs = Map((1,2) -> 4, (1,3) -> 1, (3,1) -> -1, (3,2) -> 2, (3,4) -> 3, (4,3) -> 1, (2,5) -> 1, (5,4) -> -2)
    )
    mcf.checkForNegativeCycle(p2) should be (None)
  }

  // Example from the paper "Primal method for minimal cost flow"
  "the mincostflow" should "assign correct label pi(i) and predecessors to nodes in G(X)" in {
    val (pi, pred) = mcf.label(g_of_x, 5)
    (pi(5), pred(5)) should be ((-2, Set(2)))
    (pi(2), pred(2)) should be ((-3, Set(3)))
    (pi(3), pred(3)) should be ((-5, Set(4,1)))
    (pi(4), pred(4)) should be ((-2, Set(3,5)))
    (pi(1), pred(1)) should be ((-6, Set(3)))
  }

  "the mincostflow" should "find the paths given a the predecessors" in {
    val pred = Map(5 -> Set(2), 2 -> Set(3), 3 -> Set(4,1), 4 -> Set(3,5), 1 -> Set(3))
    mcf.findNegativeCycle(5, pred) should be (List(5,2,3,4,5))
  }

  "the mincostflow" should "compute an improved flow given a problem, flow and negative cycle" in {
    val (_, pred) = mcf.label(g_of_x, 5)
    val cycle = mcf.findNegativeCycle(5,pred)
    val newFlow = mcf.improveFlow(g_of_x, f, cycle)
    newFlow should be (Map((1,2) -> 0, (1,3) -> 4, (2,5) -> 2, (3,2) -> 2, (3,4)-> 2, (4,5)-> 2))
  }

  "the mincostflow" should "find the minimal cost flow given a problem p and an initial flow f" in {
    val expected = Map((1,2) -> 0, (1,3) -> 4, (2,5) -> 2, (3,2) -> 2, (3,4)-> 2, (4,5)-> 2)
    val obtained = mcf.minCostFlow(p, Map((1,3) -> 4, (1,2) -> 0, (3,2) -> 0, (2,5) -> 0, (3,4) -> 4, (4,5) -> 4))
    obtained should be (expected)
  }

  "the mincostflow" should "find a maximum flow using the Ford/Fulkerson algorithm" in {
    val flow: Map[(Int, Int), Int] = mcf.fordFulkerson(p)
    // Note that this is a maximum flow, but not a minimum cost maximum flow.
    flow should be (Map((1,2) -> 2, (2,5) -> 2, (1,3) -> 4, (3,4) -> 4, (4,5) -> 4))
  }

  "the mincostflow" should "find a minimum cost maximum flow given a Problem p" in {
    mcf.minCostMaxFlow(p) should be (Map((1,3) -> 6, (3,2) -> 2, (2,5) -> 2, (3,4) -> 4, (4,5) -> 4, (1,2) -> 0))
  }

}
