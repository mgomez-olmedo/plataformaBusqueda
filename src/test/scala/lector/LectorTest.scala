package lector

import base.VariableType
import org.scalatest.funsuite.AnyFunSuite

/**
 * object with code for testing Lector functions
 */
class LectorTest extends AnyFunSuite{
   val fileName = "./data/car.pro"

   // just create an object of Lector class
   val lector : Lector = new Lector(fileName)

   /**
    * test the name of car.pro model. It must be
    * "UsedCarBuyer"
    */
   test("model name"){
      assert(lector.modelName == "UsedCarBuyer")
   }

   /**
    * test the number of variables using the tuples
    * with information about types and names. The file
    * car.pro contains information about 7 variables
    */
   test("number of variables"){
      assert(lector.tuplesTypeName.size == 7)
   }

   /**
    * test the number of random variables: it must be
    * just 3
    */
   test("number of random variables"){
      assert(lector.tuplesTypeName.filter(_._1 == VariableType.RANDOM).size == 3)
   }

   /**
    * test the number of decision variables defined in
    * car.pro. It must be just 3
    */
   test("number of decisions"){
      assert(lector.tuplesTypeName.filter(_._1 == VariableType.DECISION).size == 3)
   }

   /**
    * test the number of utility variables in the file. It
    * must be just 1
    */
   test("number of utilities"){
      assert(lector.tuplesTypeName.filter(_._1 == VariableType.UTILITY).size == 1)
   }

   /**
    * test the cardinalities for each variable
    */
   test("cardinalities"){
      val mapVariableStates = lector.stateNames

      // checks state names for FirstTestDecision
      assert(mapVariableStates("FirstTestDecision").size == 4)

      // SecondTestDecision has 2 states
      assert(mapVariableStates("SecondTestDecision").size == 2)

      // PurchaseDecision has 2 states
      assert(mapVariableStates("PurchaseDecision").size == 2)

      // FirstTestResults hast 4 states
      assert(mapVariableStates("FirstTestResults").size == 4)

      // SecondTestResults has 3 states
      assert(mapVariableStates("SecondTestResults").size == 3)

      // Cars_Conditions has 2 values
      assert(mapVariableStates("Cars_Conditions").size == 2)
   }

   /**
    * test the number of links defined for the model in
    * car.pro file. It must be just 16
    */
   test("number of links"){
      assert(lector.linksTuples.size == 16)
   }

   /**
    * checks the number of links for each variable
    */
   test("links per variable"){
      val links = lector.linksTuples

      // 5 links starting ar FirstTestDecision
      assert(links.filter(_._1 == "FirstTestDecision").size == 5)

      // 3 links starting at FirstTestResults
      assert(links.filter(_._1 == "FirstTestResults").size == 3)

      // 3 links for SecondTestDecision
      assert(links.filter(_._1 == "SecondTestDecision").size == 3)

      // 1 links from SecondTestResults
      assert(links.filter(_._1 == "SecondTestResults").size == 1)

      // 3 links from Cars_Conditions
      assert(links.filter(_._1 == "Cars_Conditions").size == 3)

      // 1 link from PurchaseDecision
      assert(links.filter(_._1 == "PurchaseDecision").size == 1)

      // 0 links from NetValue
      assert(links.filter(_._1 == "NetValue").size == 0)
   }

   /**
    * test the number of variables generated: it must be
    * 7
    */
   test("number of variable objects"){
      val objects = lector.variables
      assert(objects.size == 7)
   }

   test("method of generation of variable pairs for links"){
      val linkPairs = lector.links

      // the number of pairs must be 16
      assert(linkPairs.size == 16)
   }
}
