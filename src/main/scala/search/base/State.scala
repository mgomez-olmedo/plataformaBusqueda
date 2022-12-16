package search.base

import base.IDiagram

/**
 * class for representing states in the search space
 * @param diagram model represented by the state
 * @param stateId identifier of the state
 */
case class State(diagram : IDiagram, stateId : Long, cost : Double){
   /**
    * equals implementation limiting comparison to diagrams
    * @param obj target object
    * @return
    */
   override def equals(obj: Any): Boolean = {
      obj match{
         case state:State =>
            // checks diagrams
            diagram == state.diagram
         case _ => false
      }
   }

   /**
    * override hash code for comparing diagrams
    * @return
    */
   override def hashCode: Int = {
      diagram.hashCode()
   }
}
