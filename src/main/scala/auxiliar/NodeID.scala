package auxiliar

object NodeID {
   /**
    * initializes id to 0
    */
   var id  = 1L

   /**
    * gets a new id
    * @return
    */
   def getId : Long = {
      val prev = id

      // increments id
      id = id+1

      // return prv
      prev
   }
}
