package search.base

import lector.Lector
import org.scalatest.funsuite.AnyFunSuite

/**
 * class for testing ExploredSet functionality
 */
class ExploredSetTest extends AnyFunSuite{
   /**
    * creates explored set collection
    */
   val explored = new ExploredSet(Double.MaxValue)

   /**
    * creates and ID
    */
    val model = new Lector("./data/car.pro").diagram

   // store the model twice in the set
   explored.addUpdate(State(model, 1, model.getSize))
   explored.addUpdate(State(model, 2, model.getSize))

   /**
    * test repeated models are not included in the list
    */
   test("set size"){
      assert(explored.size == 1)
   }
}
