package search.strategies

import base.IDiagram
import search.base._

/**
 * class for performing breath first search but including
 * a closed list for explored diagrams
 * @param state initial state for starting the search
 */
class DepthFirstSearchPlusExplored(val state: IDiagram, var bound: Double) {

   /**
    * initializes a solution
    */
   var solution: Solution = null

   /**
    * creates a list for explored states
    */
   val exploredSet: ExploredSet = new ExploredSet(bound)

   /**
    * to count the number of iterations, updates and pruning
    */
   var iterations = 0
   var updates = 0
   var pruning = 0
   var jumped = 0

   /**
    * start the search process. In this solution it is needed to
    * keep the search till the end and it not required to include
    * nodes into explored set
    * @return
    */
   def search: Solution = {
      /**
       * method go for performing a recursive search on new nodes
       */
      def go(node: Node): Unit = {
         iterations += 1

         // shows information about the process
         if (iterations % 10000 == 0) {
            println("go loop - iterations: " + iterations + " explored: " + exploredSet.size + " ops: " + node.depth + " updates: " + updates + " pruning: " + pruning + " jumped: " + jumped + " bound: " + bound)
         }

         // check if the node extracted from the queue is
         // a goal node
         if (node.goal) {
            // change the solution if needed
            if (solution == null || node.pathCost < bound) {
               // compose the solution
               solution = Solution(Utils.composeSolution(node))

               // consider the new update
               updates += 1

               // shows information
               println("go loop - iterations: " + iterations + " explored: " + exploredSet.size + " ops: " + node.depth + " updates: " + updates + " pruning: " + pruning + " jumped: " + jumped + " bound: " + bound)

               // update the bound
               bound = node.pathCost

               // update the bound for the explored set
               exploredSet.updateBound(bound)
            }
         }
         else {
            // check the cost and discard the exploration if it is
            // bigger than the bound
            if(node.pathCost < bound){
               // check if the node was previously explored
               val currentState = State(node.state, node.id, node.pathCost)

               // checks the presence of currentState
               val result: (State, Boolean) = exploredSet.contains(currentState)

               // treat the current state
               if(result._1 == null){
                  exploredSet.add(currentState)
               }
               else{
                  if(result._2 == true){
                     // update the state
                     exploredSet.update(result._1, currentState)
                  }
               }

               // explores the state if required
               if(result._1 == null || result._2 == true){
                  // determine available actions
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
               else {
                  jumped += 1
               }
            }
            else {
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
      println("search end -> updates: " + updates + " jumped: " + jumped + " pruning: " + pruning + " bound: " + bound)

      // return solution
      solution
   }
}
