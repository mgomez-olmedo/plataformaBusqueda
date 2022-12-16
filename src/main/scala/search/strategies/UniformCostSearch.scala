package search.strategies

import auxiliar.Action
import base.IDiagram
import search.base._

import scala.annotation.tailrec

/**
 * class for performing best first search
 * @param state initial state for starting the search
 */
class UniformCostSearch(val state : IDiagram) {
   /**
    * frontier storing states to be explored
    */
   val frontier: PriorityQueue = new PriorityQueue

   /**
    * initializes a solution
    */
   var solution : Solution = null

   /**
    * to count the number of iterations
    */
   var iterations = 0
   var insertions = 0
   var updates = 0
   var rejections = 0

   /**
    * start the search process. In this solution it is needed to
    * keep the search till the end and it not required to include
    * nodes into explored set
    * @return
    */
   def search : Solution = {
      var repeatedInFrontier = 0
      var iterations = 0
      var insertionsUpdatesInFrontier = 0

      /**
       * method go for performing a recursive search getting nodes
       * for frontier
       */
      @tailrec
      def go : Solution = {
         iterations += 1
         if(iterations % 1000 == 0) {
            println("go loop - frontier: " + frontier.size +
               " repeated: " + repeatedInFrontier + " frontier insertions: " + insertionsUpdatesInFrontier)
         }

         // if frontier is empty, return an empty list
         // as indication of failure
         if (frontier.empty) solution
         else {
            // get the first node of the frontier
            val node = frontier.first

            // check if the node extracted from the queue is
            // a goal node
            if(node.goal){
               solution = Solution(Utils.composeSolution(node))
            }
            else{
               // get possible actions
               val actions = node.state.getActions(node.action)

               // checks each action
               actions.foreach(action => {
                  // creates the corresponding node for the action
                  val childNode = Node(node.state, node, action)

                  // add the childNode (if required) to frontier. If it is
                  // already contained, the cost must be updated if needed
                  val inserted = frontier.addOrUpdate(childNode)
                  if (!inserted) {
                     repeatedInFrontier += 1
                  }
                  else{
                     insertionsUpdatesInFrontier += 1
                  }
               })
            }

            // just keep on searching if solution is null
            if(solution == null)
               go
            else
               solution
         }
      }

      // creates the initial node for the search
      val startNode = Node(state)

      // check if the state represents a solution
      var result:Solution = null

      if(startNode.goal) solution
      else{
         // add the state to frontier if required
         frontier.addOrUpdate(startNode)

         // just call to go method
         result = go
      }

      println("iterations: " + iterations)
      println("go end - frontier: " + frontier.size +
         " repeated: " + repeatedInFrontier + " frontier insertions: " + insertionsUpdatesInFrontier)

      // return result
      result
   }
}
