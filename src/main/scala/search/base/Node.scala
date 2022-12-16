package search.base

import auxiliar.{Action, ActionType, NodeID}
import base.IDiagram

/**
 * class for representing nodes in the search space
 * @param state state represented by the node
 * @param parent parent node
 * @param action action applied to the node
 * @param pathCost cost from root to the node
 * @param depth level of depth for the node
 */
case class Node(state : IDiagram, parent : Node, action : Action, var pathCost : Double, depth : Int) {
   /**
    * gets the unique identifier of the node
    */
   val id: Long = NodeID.getId

   /**
    * override equals for do not taking into account parent,
    * parent, action or cost
    * @param obj target object to check
    * @return
    */
   override def equals(obj: Any): Boolean = {
      obj match{
         case other:Node =>
            other.state.equals(state)
         case _ => false
      }
   }

   /**
    * just check the diagram has a single variable
    * @return
    */
   def goal : Boolean = {
      state.numberOfNodes == 1
   }

   /**
    * toString method
    * @return
    */
   override def toString : String = {
      var output = "\n---------------------- Node id: " + id + " ----------------------\n"

      // add information about the state
      output += state.toString + "\n"

      // add information about parent id
      if(parent != null)
         output += "parent id: " + parent.id + "\n"
      else output += "parent id: none \n"

      // add information about the action
      output += action.toString + "\n"
      output += "pathCost: " + pathCost + "\n"
      output += "--------------------------------------------------------\n"

      // return output
      output
   }
}

/**
 * companion object
 */
object Node{
   /**
    * factory method od the class
    * @param state state represented by the node
    * @param parent parent of the node
    * @param action action to perform
    * @return
    */
   def apply(state : IDiagram, parent : Node, action : Action) : Node = {
      // just generate a new state applying the action
      val newState = state.execute(action)

      // update the cost once the operation is performed
      action.updateCost(newState.getSize)

      // creates a new node
      new Node(newState, parent, action, newState.getSize.max(parent.pathCost), parent.depth+1)
   }

   /**
    * factory method for the initial state of the search process
    * @param state target state
    */
   def apply(state : IDiagram) : Node = {
      // creates a new Node used for starting the search process
      // using a null action as start point
      val nullAction = Action(ActionType.NULL, -1, -1, state.getSize)
      new Node(state, null, nullAction, state.getSize, 0)
   }

   /**
    * method required for ordering nodes into the priority queue
    * @param node target node
    * @return
    */
   def order(node : Node) : Double = {
      1.0/node.pathCost
   }
}
