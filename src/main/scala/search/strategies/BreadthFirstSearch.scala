package search.strategies

import auxiliar.{Action, ActionType}
import base.IDiagram
import search.base._

import scala.annotation.tailrec

/**
 * class for performing breath first search
 * @param state initial state for starting the search
 */
class BreadthFirstSearch(val state : IDiagram) {
   /**
    * frontier storing states to be explored
    */
   val frontier: Fifo = new Fifo

   /**
    * initializes a solution
    */
   var solution: Solution = Solution(List(Action(ActionType.NULL, -1, -1, Double.MaxValue)))

   /**
    * to count the number of iterations
    */
   var iterations = 0

   /**
    * start the search process. In this solution it is needed to
    * keep the search till the end and it not required to include
    * nodes into explored set
    * @return
    */
   def search : Solution = {
      var repeatedInFrontier = 0
      var iterations = 0
      var updates = 0
      var insertionsUpdatesInFrontier = 0
      var result : Solution = null

      /**
       * method go for performing a recursive search getting nodes
       * for frontier
       */
      @tailrec
      def go : Solution = {
         iterations += 1
         if(iterations % 1000 == 0) {
            println("go loop - frontier: " + frontier.size +
               " repeated: " + repeatedInFrontier + " updates: " + updates)
         }

         // if frontier is empty, return an empty list
         // as indication of failure
         if (frontier.empty) solution
         else {
            // get the first node of the frontier
            val node = frontier.first

            // get possible actions
            val actions = node.state.getActions(node.action)

            // checks each action
            actions.foreach(action => {
               // creates the corresponding node for the action
               val childNode = Node(node.state, node, action)

               // check if it is a solution
               if (childNode.goal) {
                  val newSolution = Solution(Utils.composeSolution(childNode))
                  if(solution.cost > newSolution.cost) {
                     println("   prev. sol. cost: " + solution.cost + " new sol. cost: " + newSolution.cost)
                     solution = newSolution
                     updates += 1
                  }
               }
               else {
                  // add the childNode (if required) to frontier. If it is
                  // already contained, the cost must be updated if needed
                  val inserted = frontier.addOrUpdate(childNode)
                  if(!inserted){
                     repeatedInFrontier += 1
                  }
                  else{
                     insertionsUpdatesInFrontier += 1
                  }
               }
            })

            // just keep on searching
            go
         }
      }

      // creates the initial node for the search
      val startNode = Node(state)

      // check if the state represents a solution
      if(startNode.goal) solution
      else{
         // add the state to frontier if required
         frontier.addOrUpdate(startNode)

         // just call to go method
         result = go

         println("iterations: " + iterations)
         println("go end - frontier: " + frontier.size +
            " repeated: " + repeatedInFrontier + " frontier insertions: " + insertionsUpdatesInFrontier)

         // return result
         result
      }
   }
}
