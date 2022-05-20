package org.benedetto.mincostmaxflow

import scala.collection.mutable

case class Problem(
                    nodes: List[Int], // first node is the source and last node is the sink
                    caps: Map[(Int, Int), Int],
                    costs: Map[(Int,Int), Int]
) {

  def source: Int = { nodes.head }
  def sink: Int = { nodes.last }

  def cost(i: Int, j : Int): Int = {
    costs.getOrElse((i,j),0)
  }

  def caps(i: Int, j : Int): Int = {
    caps.getOrElse((i,j), 0)
  }

}

class MinCostFlow {

  def minCostMaxFlow(p: Problem): Map[(Int, Int), Int] = {
    minCostFlow(p, fordFulkerson(p))
  }

  def fordFulkerson(p: Problem): Map[(Int, Int), Int] = {
    var flow = Map[(Int, Int), Int]()
    while (true) {
      val g_of_x = computeG(p, flow)
      val path = findPath(g_of_x)
      if (path.isEmpty) return flow
      val delta : Int = pairs(path.get).map((a : (Int, Int)) => p.caps(a)).min
      val newFlow = mutable.Map[(Int, Int), Int]() ++ flow // convert to mutable map
      pairs(path.get).foreach(pair => {
        newFlow(pair) = newFlow.getOrElse(pair, 0) + delta
      })
      flow = newFlow.toMap
    }
    Map() // will never be reached
  }

  def findPath(problem: Problem): Option[List[Int]] = {
    findPathRec(problem, problem.source, List(problem.source))
  }

  def findPathRec(p: Problem, current: Int, path: List[Int]): Option[List[Int]] = {
    for (n <- p.nodes) {
      if (p.caps.contains(current, n)) {
        if (n == p.sink) return Some(path :+ n)
        if (! path.contains(n)) {
          val prec = findPathRec(p, n, path :+ n)
          if (prec.isDefined) return prec
        }
      }
    }
    None
  }

  def minCostFlow(p: Problem, initialFlow: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    var f = initialFlow
    while (true) {
      val g_of_x = computeG(p, f)
      val start: Option[Int] = checkForNegativeCycle(g_of_x)
      if (start.isEmpty) return f
      val (_, pred) = label(g_of_x, start.get)
      val cycle = findNegativeCycle(start.get, pred)
      f = improveFlow(g_of_x, f, cycle)
    }
    f // never reached, but needed for the compiler
  }

  def computeG(p: Problem, f: Map[(Int, Int), Int]): Problem = {
    val capsFirst = mutable.Map[(Int, Int), Int]()
    val costsFirst = mutable.Map[(Int, Int), Int]()
    p.nodes.foreach(i => {
      p.nodes.foreach(j => {
        if (f.getOrElse((i,j), 0) > 0) {
          capsFirst.put((j, i), f(i,j))
          costsFirst.put((j,i), - p.costs(i,j))
        }
        if ((f.getOrElse((j,i), 0) == 0) && (f.getOrElse((i,j),0) < p.caps(i,j))) {
          capsFirst.put((i,j), p.caps(i,j) - f.getOrElse((i,j),0))
          costsFirst.put((i,j), p.costs(i,j))
        }
      })
    })
    Problem(nodes = p.nodes, capsFirst.toMap, costsFirst.toMap)
  }

  def checkForNegativeCycle(p: Problem): Option[Int] = {
    var matrix = p.costs
    p.nodes.foreach(k => {
      matrix = replace(matrix, k, p.nodes)
      // check for negative value on the diagonal
      p.nodes.foreach(i => {
        if(matrix.getOrElse((i,i),5) < 0) return Some(i)
      })
    })
    None
  }

  def replace(matrix: Map[(Int,Int), Int], k: Int, nodes: List[Int]): Map[(Int, Int), Int] = {
    val replaced = mutable.Map[(Int, Int), Int]()
    nodes.foreach(i => {
      nodes.foreach(j => {
        // d_ij = min (d_ij, d_ik + d_kj)
        // if there is no entry in the matrix d_ij is infinity
        if (matrix.get(i,j).isEmpty) {
          if (!(matrix.get(i,k).isEmpty || matrix.get(k,j).isEmpty)) {
            replaced.put((i, j), matrix(i, k) + matrix(k, j))
          }
        } else {
          if (matrix.get(i,k).isEmpty || matrix.get(k,j).isEmpty) {
            replaced.put((i,j), matrix(i,j))
          } else {
            replaced.put((i, j), matrix(i, j).min(matrix(i, k) + matrix(k, j)))
          }
        }
      })
    })
    replaced.toMap
  }

  def label(p: Problem, start: Int): (Map[Int, Int], Map[Int, Set[Int]]) = {
    val pi = mutable.Map[Int, Int]()
    val pred = mutable.Map[Int, Set[Int]]()
    pi(start) = 0
    labelRec(p, start, start, pi, pred)
    (pi.toMap, pred.toMap)
  }

  private def labelRec(p: Problem, start: Int, current: Int, pi: mutable.Map[Int, Int], pred: mutable.Map[Int, Set[Int]]): Unit = {
    p.nodes.foreach(i => {
      // look at all outgoing edges from the current node
      if (p.costs.contains(current, i)) {
        if (!pi.contains(i)) {
          // i has no label yet
          pi(i) = pi(current) + p.cost(current, i)
          pred(i) = Set(current)
          if (!checkComplete(start, i, pi)) { labelRec(p, start, i, pi, pred) }
        } else {
          // i already has a label
          if (pi(current) + p.costs(current, i) == pi(i)) {
            // new and old labels are equal
            pred(i) = pred.getOrElse(i, Set()).+(current)
          } else {
            if (pi(current) + p.costs(current, i) < pi(i)) {
              // new label is smaller than old label
              pi(i) = pi(current) + p.costs(current, i)
              pred(i) = Set(current)
              if (!checkComplete(start, i, pi)) { labelRec(p, start, i, pi, pred) }
            }
          }
        }
      }
    })
  }

  private def checkComplete(start: Int, current: Int, pi: mutable.Map[Int, Int]): Boolean = {
    start == current && pi(start) < 0
  }

  def findNegativeCycle(start: Int, pred: Map[Int, Set[Int]]) : List[Int] = {
    findNegativeCycleRec(start, start, pred, List(start)).get
  }

  private def findNegativeCycleRec(start: Int, current: Int, pred: Map[Int, Set[Int]], visited: List[Int]): Option[List[Int]] = {
    pred(current).foreach(p => {
      if (p == start) return Some(visited :+ p)
      if (!visited.contains(p)) {
        val candidate = findNegativeCycleRec(start, p, pred, visited :+ p)
        if (candidate.isDefined) return candidate
      }
    })
    None
  }

  def improveFlow(p: Problem, f: Map[(Int, Int), Int], cycle: List[Int]): Map[(Int, Int), Int] = {
    val newFlow = mutable.Map[(Int, Int), Int]() ++ f // convert to mutable map
    val cyclePairs = pairs(cycle)
    val delta : Int = cyclePairs.map(pair => p.caps(pair.swap)).reduce((a, b) => a.min(b))
    cyclePairs.foreach(pair => {
      if (f.contains(pair)) {
        newFlow(pair) = f(pair) - delta
      } else {
        newFlow(pair.swap) = f.getOrElse(pair.swap, 0) + delta
      }
    })
    newFlow.toMap
  }

  private def pairs[T](l: List[T]): List[(T,T)] = {
    (1 until l.size).map(pos => (l(pos-1), l(pos))).toList
  }

}
