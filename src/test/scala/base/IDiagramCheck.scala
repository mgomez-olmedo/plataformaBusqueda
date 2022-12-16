package base

import lector.Lector

object IDiagramCheck extends App{

   // creates the lector for muySimple file
   val fileName = "./data/simple.pro"
   val lector : Lector = new Lector(fileName)

   // gets the diagram
   val diagram = lector.diagram

   // gets information about the diagram
   println("nodes: " + diagram.numberOfNodes)
   println("links: " + diagram.links.size)
   println("size: " + diagram.getSize)

   // makes kong evaluation on a copy
   val original = new IDiagram(diagram.variables, diagram.links)

   val actions = diagram.kongEvaluation
   println(actions.map(_.showActionVariables(original)).mkString("\n"))
   println("max cost: " + actions.reverse.map(_.cost).max+ " number of actions: " + actions.size)
}
