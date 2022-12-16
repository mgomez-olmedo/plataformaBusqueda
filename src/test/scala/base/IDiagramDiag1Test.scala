package base

import lector.Lector
import org.scalatest.funsuite.AnyFunSuite

class IDiagramDiag1Test extends AnyFunSuite{
   // creates the lector for muySimple file
   val fileName = "./data/diag1.pro"
   val lector : Lector = new Lector(fileName)

   // gets the diagram
   val diagram = lector.diagram

   /**
    * test the number of nodes and links in muySimple diagram
    */
   test("number of nodes of diag1"){
      println(diagram)
      assert(diagram.numberOfNodes == 23 && diagram.links.size == 34)
   }

   /**
    * test kong evaluation on muySimple diagram
    */
   test("Kong evaluation on diag1 diagram"){
      // preserve the original diagram
      val original = new IDiagram(diagram.variables, diagram.links)
      val actions = diagram.kongEvaluation

      // first at all shows actions information
      println("\nActions for kong evaluation of diag1 diagram")
      println(actions.reverse.map(_.showActionVariables(original)).mkString("\n"))
      println()

      assert(actions.map(_.cost).max == 4.853966861E9)
   }
}
