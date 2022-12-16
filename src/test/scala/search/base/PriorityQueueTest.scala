package search.base

import lector.Lector
import org.scalatest.funsuite.AnyFunSuite

/**
 * class for testing Fifo functionality
 */
class PriorityQueueTest extends AnyFunSuite{
   val fileName = "./data/car.pro"
   val lector = new Lector(fileName)
   val model = lector.diagram

   /**
    * creates the fifo
    */
   val pqueue = new PriorityQueue

   // creates two nodes with the same model but different costs
   val node1 = Node(model, null, null, 100, 0)
   val node2 = Node(model, null, null, 200, 0)

   // add both of them to the fifo
   pqueue.addOrUpdate(node1)
   pqueue.addOrUpdate(node2)

   /**
    * test add or update method
    */
   test("add or update"){
      assert(pqueue.size == 1)
      // preserved node must have the minimum cost
      assert(pqueue.first.pathCost == 100)
      // after extracting the element, no nodes in the
      // queue
      assert(pqueue.size == 0)
   }
}
