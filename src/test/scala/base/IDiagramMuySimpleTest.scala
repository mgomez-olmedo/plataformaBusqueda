package base

import lector.Lector
import org.scalatest.funsuite.AnyFunSuite

class IDiagramMuySimpleTest extends AnyFunSuite{
   // creates the lector for muySimple file
   val fileName = "./data/muysimple.pro"
   val lector : Lector = new Lector(fileName)

   // gets the diagram
   val diagram = lector.diagram

   /**
    * test the number of nodes and links in muySimple diagram
    */
   test("number of nodes of muySimple"){
      assert(diagram.numberOfNodes == 16 &&
               diagram.links.size == 28)
   }

   /**
    * test kong evaluation on muySimple diagram
    */
   test("Kong evaluation on muySimple diagram"){
      // preserve the original diagram
      val original = new IDiagram(diagram.variables, diagram.links)
      val actions = diagram.kongEvaluation

      // first at all shows actions information
      println("\nActions for kong evaluation of muySimple diagram")
      println(actions.reverse.map(_.showActionVariables(original)).mkString("\n"))
      println()

      assert(actions.map(_.cost).max == 7918)
   }
}
