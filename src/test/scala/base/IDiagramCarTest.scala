package base

import lector.Lector
import org.scalatest.funsuite.AnyFunSuite

class IDiagramCarTest extends AnyFunSuite{
   // creates the lector for car.pro file
   val fileName = "./data/car.pro"
   val lector : Lector = new Lector(fileName)

   // gets the diagram
   val diagram = lector.diagram

   /**
    * test the number of nodes
    */
   test("number of nodes for car diagram"){
      assert(diagram.numberOfNodes == 7)
   }

   /**
    * test links on car diagram
    */
   test("test links for car diagram"){
      assert(diagram.link("FirstTestDecision", "SecondTestDecision") == true)
      assert(diagram.link("FirstTestDecision", "PurchaseDecision") == true)
      assert(diagram.link("FirstTestDecision", "FirstTestResults") == true)
      assert(diagram.link("FirstTestDecision", "SecondTestResults") == true)
      assert(diagram.link("FirstTestDecision", "NetValue") == true)
      assert(diagram.link("FirstTestResults", "SecondTestDecision") == true)
      assert(diagram.link("FirstTestResults", "PurchaseDecision") == true)
      assert(diagram.link("FirstTestResults", "SecondTestResults") == true)
      assert(diagram.link("SecondTestDecision", "SecondTestResults") == true)
      assert(diagram.link("SecondTestDecision", "PurchaseDecision") == true)
      assert(diagram.link("SecondTestDecision", "NetValue") == true)
      assert(diagram.link("SecondTestResults", "PurchaseDecision") == true)
      assert(diagram.link("Cars_Conditions", "FirstTestResults") == true)
      assert(diagram.link("Cars_Conditions", "SecondTestResults") == true)
      assert(diagram.link("Cars_Conditions", "NetValue") == true)
      assert(diagram.link("PurchaseDecision", "NetValue") == true)
   }

   /**
    * checks computation of diagram size: 258 for car.pro
    */
   test("test size of car diagram"){
      assert(diagram.getSize == 258)
   }

   /**
    * checks kong evaluation of car.pro. In mut be 336.
    * There are 9 actions (8 + initial one)
    */
   test("kong evaluation for car diagram"){
      // preserve the original diagram for showing actions
      val original = new IDiagram(diagram.variables, diagram.links)
      val actions = diagram.kongEvaluation

      // show actions
      println("\nActions for kong evaluation of car diagram")
      println(actions.reverse.map(_.showActionVariables(original)).mkString("\n"))
      println()
      assert(actions.map(_.cost).max == 336 && actions.size == 9)
   }
}
