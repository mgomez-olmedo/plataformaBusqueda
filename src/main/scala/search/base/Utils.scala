package search.base

import auxiliar.Action

import scala.annotation.tailrec

/**
 * object for storing utility methods
 */
object Utils {
   /**
    * compose the solution considering all the nodes until
    * reaching the root
    * @param node solution node used for composing the list
    *             of actions leading to the solution
    * @return
    */
   def composeSolution(node: Node) : List[Action] = {
      @tailrec
      def go(node : Node, history : List[Action]) : List[Action] = {
         if(node.parent == null) history
         else{
            go(node.parent, node.action :: history)
         }
      }

      // just start the process
      go(node, List())
   }
}
