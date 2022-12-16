package base

/**
 * class for storing variables information
 * @param id identifier of variable
 * @param name name of the variable
 * @param kind kind of the variable: decision, chance node or utility
 * @param states list of states assigned to the variable
 */
class Variable(val id : Int, val name : String, val kind : VariableType,
               val states : List[String]) {
   /**
    * cardinality of the variable
    * @return
    */
   def cardinality : Int = {
      if (states.nonEmpty) {
         // for random and decision nodes
         states.size
      }
      else{
         // special case for utility nodes
         1
      }
   }
}

/**
 * companion object
 */
object Variable{
   /**
    * factory method for variables
    * @param id identifier of the variable
    * @param name name of the variable
    * @param kind kind of variable: decision, chance or utility node
    * @param states set of states assigned to the variable
    * @return
    */
   def apply(id : Int, name : String, kind : VariableType, states : List[String]): Variable = {
      new Variable(id, name, kind, states)
   }
}
