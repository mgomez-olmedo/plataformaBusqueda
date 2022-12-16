package auxiliar

import base.IDiagram

case class Action(kind : ActionType, id1 : Int, id2 : Int,
                  var cost : Double = -1){

   /**
    * shows action information including variable names
    * @param diagram target influence diagram 
    */
   def showActionVariables(diagram : IDiagram): String = {
      var output  = kind.toString + " "
      if(kind != ActionType.NULL) {
         output += diagram.getVariable(id1).name + " "
         if (kind == ActionType.REVERSAL) {
            output += " -> " + diagram.getVariable(id2).name + " "
         }
         else output += ""
      }
      else output += ""

      // finally add cost info
      output += cost

      // return output
      output
   }

   /**
    * update the cost of the action
    * @param newCost updated cost for the action
    */
   def updateCost(newCost : Double) : Unit = {
      cost = newCost
   }
}
