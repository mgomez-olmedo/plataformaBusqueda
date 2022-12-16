package search.base

import scala.collection.mutable

/**
 * class for storing information with a priority queue
 * style
 */
class PriorityQueue {
   /**
    * collection for nodes in frontier
    */
   val pQueue: mutable.PriorityQueue[Node] = 
      mutable.PriorityQueue[Node]()(Ordering.by(Node.order))

   /**
    * insert a new node if needed or just update de information
    * of cost if the node is already contained into the collection
    * @param node target node for the update
    */
   def addOrUpdate(node : Node): Boolean = {
      val prevNodeOption = pQueue.find(x => x == node)
      if(prevNodeOption.isEmpty){
         // just add it to the pQueue
         pQueue.enqueue(node)
         true
      }
      else{
         val prevNode = prevNodeOption.get
         if(prevNode.pathCost > node.pathCost){
            // remove prevNode and insert node
            pQueue.filter(x => x != prevNode)

            // and now add node
            pQueue.enqueue(node)
            true
         }
         else{
            false
         }
      }
   }

   /**
    * checks if the queue is empty
    * @return
    */
   def empty : Boolean = {
      pQueue.isEmpty
   }

   /**
    * extracts the first element
    * @return
    */
   def first : Node = {
      pQueue.dequeue()
   }

   /**
    * return the number of nodes into the queue
    * @return
    */
   def size: Int = pQueue.size
}
