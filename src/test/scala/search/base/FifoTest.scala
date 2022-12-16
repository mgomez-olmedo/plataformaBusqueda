package search.base

import lector.Lector
import org.scalatest.funsuite.AnyFunSuite

/**
 * class for testing Fifo functionality
 */
class FifoTest extends AnyFunSuite{
   val fileName = "./data/car.pro"
   val lector = new Lector(fileName)
   val model = lector.diagram

   /**
    * creates the fifo
    */
   val fifo = new Fifo

   // creates two nodes with the same model but different costs
   val node1 = Node(model, null, null, 100, 0)
   val node2 = Node(model, null, null, 200, 0)

   // add both of them to the fifo
   fifo.addOrUpdate(node1)
   fifo.addOrUpdate(node2)

   /**
    * test add or update method
    */
   test("add or update"){
      assert(fifo.size == 1)
      // preserved node must have the minimum cost
      assert(fifo.first.pathCost == 100)
   }
}
