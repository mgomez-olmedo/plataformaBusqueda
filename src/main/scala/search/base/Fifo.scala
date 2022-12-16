package search.base

import base.IDiagram

import scala.collection.mutable.ArrayBuffer

/**
 * class for storing nodes in a first-in-first-out
 * style
 */
class Fifo {
   /**
    * list of states contained into the FIFO list
    */
   var nodes : ArrayBuffer[Node] = ArrayBuffer()

   /**
    * add the state to the list of states if needed
    * @param node target node to add or check for update
    * @return
    */
   def addOrUpdate(node : Node) : Boolean = {
      val index = nodes.indexOf(node)
      if(index != -1){
         // preserve the one with minimum path cost
         if(node.pathCost < nodes(index).pathCost){
            // replace inserted node by the one passed as argument
            nodes.remove(index)
            nodes.insert(index, node)

            // return true
            true
         }
         else {
            false
         }
      }
      else{
         // add the state
         nodes += node
         true
      }
   }

   /**
    * checks if the node passed as argument is already
    * contained
    * @param node target node to check
    * @return
    */
   def contains(node : Node) : Boolean = {
      nodes.contains(node)
   }

   /**
    * check if the fifo is empty
    * @return
    */
   def empty : Boolean = {
      nodes.isEmpty
   }

   /**
    * gets the first element of the fifo
    * @return
    */
   def first : Node = {
      nodes.remove(0)
   }

   /**
    * return the size of the collection
    * @return
    */
   def size : Long = nodes.size

   /**
    * toString method
    * @return
    */
   override def toString: String = {
      var output = "\n------------------ FIFO nodes ----------------\n"
      output += nodes.map(_.id).mkString(" - ") + "\n"
      output += "---------------------------------------------------\n"

      // return output
      output
   }
}
