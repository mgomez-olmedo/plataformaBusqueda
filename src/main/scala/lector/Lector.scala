package lector

import base.{IDiagram, Variable, VariableType}

import scala.io.Source

/**
 * class for processing information with pro syntax
 * @param fileName name of the file to read
 */
class Lector(val fileName : String) {
   /**
    * content of the file: each line is a string
    */
   val content: List[String] = readFileContent

   /**
    * get the name of the model
    */
   val modelName: String = getModelName

   /**
    * get tuples with type and name for all the variables
    */
   val tuplesTypeName: List[(VariableType, String)] = getTypeNameTuples

   /**
    * get tge list of state names for each variable
    */
   val stateNames : Map[String, List[String]] = getStateNames

   /**
    * gets information about tuples with start and end of links
    */
   val linksTuples: List[(String, String)] = getStartEndLinkTuples

   /**
    * compose the list of variables contained in the model
    */
   val variables: List[Variable] = generateVariables

   /**
    * compose the adjacency matrix with links information
    */
   val links: List[(Variable, Variable)] = generateLinksVariablePairs

   /**
    * creates the diagram with the information of variables and
    * links
    */
   val diagram : IDiagram = IDiagram(variables, links)

   /**
    * open the file, read the content and return a list
    * with file lines as strings
    * @return
    */
   private def readFileContent: List[String] = {
      // generates the file for reading the data
      val file = Source.fromFile(fileName)

      // read all the lines
      val content = file.getLines.filter(line => line.startsWith("0") ||
                                         line.startsWith("1 ") ||
                                         line.startsWith("2") ||
                                         line.startsWith("3")).toList

      // close the file
      file.close

      // return the content
      content
   }

   /**
    * process the content looking for model name
    * @return
    */
   private def getModelName : String = {
      content.filter(_.startsWith("0")).head.split(" ")(1)
   }

   /**
    * return tuples with type of node and name for each variable
    * @return
    */
   private def getTypeNameTuples: List[(VariableType, String)] = {
      content.filter(_.startsWith("1")).
         map(line => line.split(" ")).
         map(lineContent => (VariableType.fromOrdinal(lineContent(2).toInt), lineContent(4)))
   }

   /**
    * method get state names for each variable
    */
   private def getStateNames = {
      // step1: gets all pairs defining state names
      val step1: Seq[(String, String)] = content.filter(_.startsWith("2")).
         map(_.split(" ")).map(line => (line(1), line(2)))

      // step 2: group pairs by variable and creates a map with
      // variable name as key and variable state names as value.
      // This is just the result provided by this method
      step1.groupBy(_._1).
         map(entry => (entry._1, entry._2.map(_._2).toList))
   }

   /**
    * process information about links and return a list with
    * tuples with (start - end) of links
    * @return
    */
   private def getStartEndLinkTuples: List[(String, String)] = {
      content.filter(_.startsWith("3")).map(_.split(" ")).
         map(link => (link(2), link(1)))
   }

   /**
    * generate variable objects
    * @return
    */
   private def generateVariables: List[Variable] = {
      // generates the objects with variables information
      // get variable names
      val variableNames: List[String] = tuplesTypeName.map(_._2)

      // for each variable gather all its information
      variableNames.indices.map(id => {
         // gets variable name
         val variableName = variableNames(id)

         // gets the kind
         val kind : VariableType = tuplesTypeName.
            filter(tuple => tuple._2 == variableName).head._1

         // gets the states except for utility
         val states = if(kind != VariableType.UTILITY) {
            stateNames(variableName)
         }
         else{
            // for utility nodes return an empty list of states
            List()
         }


         // now make the variable
         Variable(id+1, variableName, kind, states)
      }).toList
   }

   /**
    * generate ones values for the adjacency matrix corresponding
    * to links information
    * @return
    */
   private def generateLinksVariablePairs: List[(Variable, Variable)] = {
      // consider links for starting at each variable and makes the
      // list of pairs
      variables.flatMap(variable => {
         // gets the links for this variable
         val linksStartingOnVariable: List[Variable] = linksTuples.filter(_._1 == variable.name).
            map(_._2).map(name => variables.find(_.name == name).get)

         // now makes all pairs for variable
         linksStartingOnVariable.map(end => (variable, end))
      })
   }
}
