package search.strategies

import base.IDiagram
import search.base._

/**
 * class for performing breath first search
 * @param state initial state for starting the search
 */
class DepthFirstSearch(val state : IDiagram, var bound : Double) {

   /**
    * initializes a solution
    */
   var solution : Solution = null

   /**
    * to count the number of iterations, updates and pruning
    */
   var iterations = 0
   var updates = 0
   var pruning = 0


   /**
    * start the search process. In this solution it is needed to
    * keep the search till the end and it not required to include
    * nodes into explored set
    * @return
    */
   def search : Solution = {

      /**
       * method go for performing a recursive search on new nodes
       */
      def go(node : Node) : Unit  = {
         iterations += 1

         // shows information about the process
         if(iterations % 1000 == 0) {
            println("go loop - iterations: " + iterations+ " updates: " + updates + " pruning: " + pruning + " bound: " + bound)
         }

         // check if the node extracted from the queue is
         // a goal node
         if(node.goal){
            // change the solution if needed
            if(solution == null || node.pathCost < bound) {
               // compose the solution
               solution = Solution(Utils.composeSolution(node))

               // consider the new update
               updates += 1

               // shows information
               println("go loop - iterations: " + iterations+ " updates: " + updates + " pruning: " + pruning + " bound: " + bound)

               // update the bound
               bound = node.pathCost
            }
         }
         else{
            // discard the processing if the bound is improved
            if(node.pathCost <= bound) {
               // get possible actions
               val actions = node.state.getActions(node.action)

               // checks each action
               actions.foreach(action => {
                  // creates the corresponding node for the action
                  val childNode = Node(node.state, node, action)

                  // if the cost of the node is under the bound, keeps
                  // on analyzing
                  go(childNode)
               })
            }
            else{
               // update the number of pruning
               pruning += 1
            }
         }
      }

      // creates the initial node for the search
      val startNode = Node(state)

      // just call to go method
      go(startNode)

      println("iterations: " + iterations)
      println("search end -> updates: " + updates + " pruning: " + pruning + " bound: " + bound)

      // return solution
      solution
   }
}
