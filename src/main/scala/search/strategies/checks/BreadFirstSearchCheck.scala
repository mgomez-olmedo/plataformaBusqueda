package search.strategies.checks

import base.IDiagram
import lector.Lector
import search.strategies.BreadthFirstSearch

/**
 * class for checking the breath first search
 */
object BreadFirstSearchCheck{
   def main(args : Array[String]) = {
      if (args.isEmpty) {
         println("error in program call: needed file to evaluate")
      }
      else {
         // creates the lector for muySimple file
         val fileName = args(0)
         val lector: Lector = new Lector(fileName)

         // gets the diagram
         val diagram = lector.diagram

         // makes a copy for getting Kong evaluation
         val copyDiagram = new IDiagram(diagram.variables, diagram.links)
         val kong = copyDiagram.kongEvaluation
         println("kong evaluation: " + kong)
         println("kong estimation: " + kong.map(_.cost).max)

         // gets information about the diagram
         println("nodes: " + diagram.numberOfNodes)
         println("links: " + diagram.links.size)
         println("size: " + diagram.getSize)

         // creates object for breath first search
         val searcher = new BreadthFirstSearch(diagram)

         // just call the search method
         val result = searcher.search
         println("max cost: " + result.cost)
         println("result:  " + result)
      }
   }
}
