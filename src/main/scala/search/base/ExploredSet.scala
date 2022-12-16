package search.base

import scala.collection.mutable.Set
import base.IDiagram

import scala.collection.mutable

/**
 * class for storing the set of states used during exploration
 */
class ExploredSet(var bound : Double) {
   /**
    * sets of explored diagrams
    */
   var explored: mutable.Set[State] = mutable.Set[State]()

   /**
    * add a new state to the set or update its bound if present
    *
    * @param state state to add to the set of explored states
    */
   def addUpdate(state: State): Unit = {
      val prev = contains(state)
      if(prev._1 != null){
         if(prev._1.cost > state.cost) {
            // remove the state from the set
            explored -= prev._1

            // add the new one
            explored += state
         }
      }
      else{
         if(state.cost <= bound)
            explored += state
      }
   }

   /**
    * update the state
    * @param state
    */
   def update(prevState : State, newState : State) : Unit = {
      // remove the previous state from the set
      explored -= prevState

      // add the new one
      explored += newState
   }

   /**
    * adds a new state if needed
    * @param state
    */
   def add(state : State): Unit = {
      if (state.cost <= bound)
         explored += state
   }

   /**
    * checks if the set contains an object with the same or lower
    * cost than the state passed as argument
    *
    * @param state state to check
    * @return
    */
   def contains(state : State) : (State, Boolean) = {
      var result : (State, Boolean) = (null, false)
      val prev = explored.find(prev => prev.diagram == state.diagram)
      if(prev.isDefined) {
         if (prev.head.cost <= state.cost) {
            result = (prev.head, false)
         }
         else {
            result = (prev.head, true)
         }
      }

      // return result
      result
   }

   /**
    * determine the size of the set
    * @return
    */
   def size : Long = {
      explored.size
   }

   /**
    * classify the states according to number of nodes
    * (first key) and number of links (second key)
    * @return
    */
   private def getStatistics : Map[Int, Map[Int, mutable.Set[IDiagram]]] = {
      val byNodes: Map[Int, mutable.Set[IDiagram]] =
         explored.map(_.diagram).groupBy(_.numberOfNodes)
      byNodes.map(entry => {
         val byLinks: Map[Int, mutable.Set[IDiagram]] =
            entry._2.groupBy(_.links.size)
         (entry._1, byLinks.map(entry => (entry._1, entry._2)))
      })
   }

   /**
    * remove states with cost over bound
    * @param bound target bound for the prune
    */
   def purge(bound : Double): Unit = {
      val prevSize = explored.size
      explored = explored.filter(_.cost > bound)
      val newSize = explored.size
      println("purged: " + (prevSize - newSize))
   }

   /**
    * updates the bound
    * @param bound target bound for the update
    */
   def updateBound(bound : Double): Unit = {
      this.bound = bound

      // purge states
      // purge(bound)
   }

   /**
    * toString method
    * @return
    */
   override def toString: String = {
      var output = "\n-------------- explored set -----------------\n"
      output += "ids of related nodes: "
      output += explored.map(_.stateId).mkString(" - ") + "\n"
      output += "\nStatistics of explored set\n"
      output += "number of states: " + explored.size + "\n"
      val statistics = getStatistics
      statistics.foreach(entry => {
         output += " number of nodes: " + entry._1 + "\n"
         val models = entry._2
         models.foreach(byLinks => {
            output += "  links: " + byLinks._1 + " models: " + byLinks._2.size + "\n"
            if(byLinks._2.size > 5){
               output += "-------------------------------------------------\n"
               output += byLinks._2.mkString(" ")
               output += "-------------------------------------------------\n"
            }
         })
      })
      output += "----------------------------------------\n"

      // return output
      output
   }
}
