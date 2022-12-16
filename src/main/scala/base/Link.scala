package base

/**
 * case class for representing links between variables
 * @param variable1 source of the link
 * @param variable2 destination of the link
 */
case class Link(variable1 : Variable, variable2 : Variable){
   /**
    * override equals for testing equality of variables
    * @param obj object to compare with
    * @return
    */
   override def equals(obj: Any): Boolean =  {
      obj match{
         case link : Link =>
            variable1.id == link.variable1.id && variable2.id == link.variable2.id
         case _ => false
      }
   }
}
