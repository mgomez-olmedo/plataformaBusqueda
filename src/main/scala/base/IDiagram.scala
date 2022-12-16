package base

import auxiliar.{Action, ActionType}
import base.IDiagram.contains

import scala.annotation.tailrec

/**
 * class for storing IDiagram information
 *
 * @param variables list of variables assigned to the diagram
 * @param links     list of links defining the relations between the variable
 */
class IDiagram(val variables: List[Variable], val links: List[Link]) {
   /**
    * sets the relation between variable name and index into
    * variables list
    */
   val mapVariableNameIndex: Map[String, Int] = generateMapVariableNameIndex

   /**
    * sets the relation between id of variable (key) and index into
    * variables list (value)
    */
   val mapVariableIdIndex: Map[Int, Int] = generateMapVariableIdIndex

   /**
    * matrix storing links information
    */
   val matrix: AdjacencyMatrix = AdjacencyMatrix(variables.size, generateOnes)

   /**
    * return the number of nodes
    *
    * @return
    */
   val numberOfNodes: Int = variables.size

   /**
    * stores the id of the utility node
    */
   val utilityNodeId: Int = getUtilityNodeId

   /**
    * compares two diagrams
    *
    * @param obj target object to compare with
    * @return
    */
   override def equals(obj: Any): Boolean = {
      obj match {
         case other: IDiagram =>
            if (other.numberOfNodes != numberOfNodes ||
               other.links.size != links.size || other.getSize != getSize) false
            else {
               // required links comparison
               val result = links.diff(other.links)
               // return the comparison of result with empty
               result.isEmpty
            }
         case _ => false
      }
   }

   /**
    * execute an operation and produces a new IDiagram as
    * a result
    *
    * @param action action to perform on the diagram
    * @return
    */
   def execute(action: Action): IDiagram = {
      val finalDiagram = action.kind match {
         case ActionType.DELETION => removeVariable(action.id1)
         case ActionType.REVERSAL => revertArc(action.id1, action.id2)
         case _ => this
      }

      // detect sinks and remove if required
      val withoutSinks = finalDiagram.removeSinkVariables()

      // add the new action to history and return
      withoutSinks
   }

   /**
    * sets relations between ids (keys) and indices (values)
    *
    * @return
    */
   private def generateMapVariableIdIndex = {
      variables.indices.map(index => (variables(index).id, index)).toMap
   }

   /**
    * generate a map with variable names (key) and index in
    * list of variables (value)
    *
    * @return
    */
   private def generateMapVariableNameIndex = {
      variables.indices.map(index => (variables(index).name, index)).toMap
   }

   /**
    * generates the pair of ones defining links in diagram
    *
    * @return list of pairs of indices for start and end variables
    */
   private def generateOnes = {
      links.map(link => {
         val indexStart = variables.indexOf(link.variable1)
         val indexEnd = variables.indexOf(link.variable2)
         (indexStart, indexEnd)
      })
   }

   /**
    * produces the list of available actions of the state
    *
    * @param last last action performed, required for determining
    *             in reversal operations must be considered for
    *             a given variable
    * @return
    */
   def getActions(last: Action): List[Action] = {
      // if the last operation was a reversal, then try to
      // find another reversals for the same origin
      if (last.kind == ActionType.REVERSAL) {
         val actions = getReversals(last.id1)
         if (actions.isEmpty) {
            List(Action(ActionType.DELETION, last.id1, -1))
         }
         else actions
      }
      else {
         // try to find deletions of random variables
         val actions = getRemovableRandoms
         if (actions.nonEmpty) {
            actions
         }
         else {
            // try to find deletions of decision variables
            val actions = getRemovableDecisions
            if (actions.nonEmpty) {
               actions
            }
            else {
               // get reversal operations
               getReversals()
            }
         }
      }
   }

   /**
    * gets the variable given its id
    *
    * @param id identifier of the variable to retrieve
    * @return
    */
   def getVariable(id: Int): Variable = {
      variables(mapVariableIdIndex(id))
   }

   /**
    * gets the variables corresponding to a certain
    * kind
    *
    * @param kind target kind of variables to retrieve
    */
   private def getVariables(kind: VariableType): List[Variable] = {
      variables.filter(_.kind == kind)
   }

   /**
    * gets the decision variables ready for removal
    *
    * @return
    */
   def getRemovableDecisions: List[Action] = {
      // get decision variables
      val decisionVariables = getVariables(VariableType.DECISION)

      // filter those having another decision as successors
      val candidateDecision = decisionVariables.filter(decision => {
         // get successors
         val decisionSuccessors = successors(decision).filter(_.kind == VariableType.DECISION)

         // check if there are not decision as successors
         decisionSuccessors.isEmpty
      })

      // now consider the last condition: parents of candidate must be
      // parents of utility variable as well. There must be a single candidate
      val candidate = candidateDecision.head
      val candidateParents = parents(candidate)
      val filteredParents = candidateParents.filter(parent =>
         hasSuccessor(parent, getVariable(utilityNodeId)))

      // check that candidateParents and filteredParents
      // has the same size
      if (filteredParents.size == candidateParents.size) {
         // makes an action for this deletion
         val action = Action(ActionType.DELETION, candidate.id, -1, -1)
         List(action)
      }
      else {
         List()
      }
   }

   /**
    * gets the list of variables corresponding to random variables ready
    * for deletion
    *
    * @return
    */
   private def getRemovableRandoms: List[Action] = {
      // gets random variables
      val randomVariables = getVariables(VariableType.RANDOM)

      // filter those having the utility node as single successor
      randomVariables.filter(variable => singleSuccessor(variable, getVariable(utilityNodeId))).
         map(candidate => Action(ActionType.DELETION, candidate.id, -1))
   }

   /**
    * determine available reversal operations. The result is a
    * map with id of origin variable as key and the list of
    * reversal operations having this variable as origin as value
    *
    * @return
    */
   private def getReversals(): List[Action] = {
      // get random variables being parents of utility node
      val candidates =
         parents(getVariable(utilityNodeId)).
            filter(_.kind == VariableType.RANDOM).
            filter(variable => {
               val varSuccessors = successors(variable)
               !contains(varSuccessors, VariableType.DECISION)
            })

      // check origins and get possible destinations: only those
      // with a single path between origin and destination are
      // valid
      candidates.flatMap(origin => {
         // determine random variables as successors
         val candidateSuccessors =
            successors(origin).filter(_.kind == VariableType.RANDOM)

         // the reversal is possible if and only if there is a
         // single path between origin and dest
         // gets indices for origin
         val originIndex = mapVariableIdIndex(origin.id)
         val validDestinations =
            candidateSuccessors.filter(destination =>
               matrix.singlePath(originIndex, mapVariableIdIndex(destination.id)))

         val reversals = validDestinations.map(destination =>
            Action(ActionType.REVERSAL, origin.id, destination.id))

         // just return the list of reversals
         reversals
      })
   }

   /**
    * determine available reversal operations focused on a
    * given origin
    *
    * @return
    */
   private def getReversals(id: Int): List[Action] = {
      // gets origin variable
      val origin = variables(mapVariableIdIndex(id))

      // determine random variables as successors
      val originSuccessors =
         successors(origin).filter(_.kind == VariableType.RANDOM)

      // the reversal is possible if and only if there is a
      // single path between origin and dest
      // gets indices for origin and determine valid destinations
      val originIndex = mapVariableIdIndex(origin.id)
      val validDestinations = originSuccessors.filter(destination =>
         matrix.singlePath(originIndex, mapVariableIdIndex(destination.id)))

      // compose and return actions for each valid destination
      validDestinations.map(destination =>
         Action(ActionType.REVERSAL, origin.id, destination.id))
   }

   /**
    * return the list of sink nodes
    *
    * @return
    */
   private def getSinks: List[Variable] = {
      variables.filter(_.kind != VariableType.UTILITY).filter(variable => {
         val variableSuccessors = successors(variable)
         variableSuccessors.isEmpty
      })
   }

   /**
    * gets the complete size of the diagram
    *
    * @return
    */
   def getSize: Double = {
      variables.filter(_.kind != VariableType.DECISION).map(variable => {
         val variableParents = parents(variable)
         // gets parents and gets cardinalities
         (variable.cardinality.toDouble :: variableParents.map(_.cardinality.toDouble)).product
      }).sum
   }

   /**
    * gets the index of the utility node
    *
    * @return
    */
   private def getUtilityNodeId: Int = {
      variables.filter(_.kind == VariableType.UTILITY).head.id
   }

   /**
    * override hash code for comparing diagrams
    *
    * @return
    */
   override def hashCode: Int = {
      val variablesString = variables.sortBy(_.name).map(_.name).mkString("")
      val variablesHash = variablesString.hashCode
      val linksString = links.map(link => link.variable1.name + link.variable2.name).
         sorted.mkString("")
      val linksHash = linksString.hashCode
      variablesHash + linksHash
   }

   /**
    * checks if the origin variable has destination as
    * successor
    *
    * @param origin      target node to look for links, as origin
    * @param destination target node to look for links, as destination
    * @return
    */
   private def hasSuccessor(origin: Variable, destination: Variable): Boolean = {
      // get successors of origin
      val originSuccessors = successors(origin)

      // check if destination is contained in successors
      originSuccessors.contains(destination)
   }

   /**
    * method for estimating evaluation with Kong heuristic
    *
    * @return
    */
   def kongEvaluation: List[Action] = {
      /*
       * auxiliary method for recursive execution
       *
       * @param diagram target diagram
       * @param history list of actions performed on the diagram
       * @return
       */
      @tailrec
      def go(diagram: IDiagram, history: List[Action]): List[Action] = {
         // base case: just one variable
         if (diagram.numberOfNodes == 1) history
         else {
            // get available actions: the last operation is at the
            // beginning of the history
            val actions = diagram.getActions(history.head)

            // select the best alternative according to Kong
            val best = diagram.selectAndExecuteBest(actions)

            // keeps on evaluating the model
            go(best._1, best._2 :: history)
         }
      }

      // just start the process
      go(this, List(Action(ActionType.NULL, -1, -1,
         getSize)))
   }

   /**
    * check if a link exist
    *
    * @param start target node for the check as origin
    * @param end target node for the check as destination
    * @return
    */
   def link(start: String, end: String): Boolean = {
      // get indices of variables
      val indexStart = mapVariableNameIndex(start)
      val indexEnd = mapVariableNameIndex(end)

      // checks the value stored in this position
      matrix.one(indexStart, indexEnd)
   }

   /**
    * gets parent variables of another passed as argument
    *
    * @param variable target variable for parents search
    * @return
    */
   def parents(variable: Variable): List[Variable] = {
      // get index of variable
      val colIndex = mapVariableIdIndex(variable.id)

      // gets indices with links having variable as destination
      (0 until numberOfNodes).map(rowIndex => {
         if (matrix.one(rowIndex, colIndex)) variables(rowIndex) else null
      }).filter(_ != null).toList
   }

   /**
    * private method for removing sink variables
    * @return
    */
   private def removeSinkVariables(): IDiagram = {
      // auxiliary method for deleting sinks
      @tailrec
      def go(diagram: IDiagram, sinks: List[Variable]): IDiagram = {
         if (sinks.isEmpty) diagram
         else {
            go(removeVariable(sinks.head.id), sinks.tail)
         }
      }

      val sinks = getSinks

      // just start the process
      go(this, sinks)
   }

   /**
    * removes a variable a produces a new IDiagram
    *
    * @param id identifier of the target variable
    */
   def removeVariable(id: Int): IDiagram = {
      // get the variable to remove
      val variableToRemove = getVariable(id)

      // filter variables
      val newVariables = variables.filter(_.id != id)

      // filter links
      val newLinks = links.filter(link => link.variable1.id != id && link.variable2.id != id)

      // if the variable is a random one, add its parents to the
      // value node
      val newLinksToUtility = if (variableToRemove.kind == VariableType.RANDOM) {
         val utility = getVariable(utilityNodeId)

         // get parents of variableToRemove
         val parentsRandomVariable = parents(variableToRemove)

         // add links between parent and utility
         parentsRandomVariable.map(parent => Link(parent, utility))
      }
      else List()

      // just produce a new IDiagram
      new IDiagram(newVariables, (newLinks ::: newLinksToUtility).distinct)
   }

   /**
    * method for arc reversal: it produces a new IDiagram
    *
    * @param id1 origin of the arc to revert
    * @param id2 destination of the arc to revert
    * @return
    */
   def revertArc(id1: Int, id2: Int): IDiagram = {
      val variable1 = getVariable(id1)
      val variable2 = getVariable(id2)

      // get parents for id1
      val variable1Parents = parents(variable1)
      val variable2Parents = parents(variable2).filter(_ != variable1)

      // add id1Parents to variable2
      val newVariable2Links = variable1Parents.map(parent => Link(parent, variable2))

      // add variable2Parents to variable1
      val newVariable1Links = variable2Parents.map(parent => Link(parent, variable1))

      // remove the link from variable1 to variable2
      val remainingLinks = links.filter(link => !link.equals(Link(variable1, variable2)))

      // add link from variable2 to variable1
      val revertedLink = Link(variable2, variable1)

      // now compose the new IDiagram
      new IDiagram(variables, (revertedLink :: remainingLinks ::: newVariable1Links ::: newVariable2Links).distinct)
   }

   /**
    * selects the action with minimal cost
    *
    * @param actions list of available actions
    * @return tuple with diagram and action
    */
   private def selectAndExecuteBest(actions: List[Action]): (IDiagram, Action) = {
      val selectedPair = actions.map(action =>
         // execute the action and get the result
         (execute(action), action)).minBy(_._1.getSize)

      // creates a new action for returning the result
      val selectedAction = Action(selectedPair._2.kind, selectedPair._2.id1,
         selectedPair._2.id2, selectedPair._1.getSize)

      // return the diagram and the corresponding action once the cost
      // was updated
      (selectedPair._1, selectedAction)
   }

   /**
    * checks if variable has a single successor another one
    *
    * @param start origin node for checking the condition of
    *              having a single successor
    * @param end destination node for checking the condition
    * @return
    */
   private def singleSuccessor(start: Variable, end: Variable): Boolean = {
      // gets successors of id1
      val targetSuccessors = successors(start)

      // checks there is a single successor and this matches id2
      targetSuccessors.size == 1 && targetSuccessors.head.id == end.id
   }

   /**
    * gets successors variables of another passed as argument
    *
    * @param variable target variable
    * @return
    */
   private def successors(variable: Variable): List[Variable] = {
      // get index of variable
      val rowIndex = mapVariableIdIndex(variable.id)

      // gets indices with links having index as start
      (0 until numberOfNodes).map(colIndex => {
         if (matrix.one(rowIndex, colIndex)) variables(colIndex) else null
      }).filter(_ != null).toList
   }

   /**
    * toString method
    *
    * @return
    */
   override def toString: String = {
      var output = "-------------------------------------\n"
      output += "number of nodes: " + numberOfNodes + "\n"
      output += "number of links: " + links.size + "\n"
      val sortedLinks = links.sortWith((l1, l2) => l1.variable1.name < l2.variable1.name)
      output += sortedLinks.map(link => link.variable1.name + " -> " + link.variable2.name).mkString("\n")
      output += "\nsize : " + getSize
      output += "\n-------------------------------------\n"
      output
   }
}

/**
 * companion object
 */
object IDiagram {
   /**
    * factory method
    *
    * @param variables variables of the diagram
    * @param links links of the diagram
    * @return
    */
   def apply(variables: List[Variable], links: List[(Variable, Variable)]): IDiagram = {
      val linkObjects = links.map(pair => Link(pair._1, pair._2))
      new IDiagram(variables, linkObjects)
   }

   /**
    * checks if a list of variables contains one of the kind
    * passed as argument
    *
    * @param list list of target variables
    * @param kind target kind to check for
    * @return
    */
   def contains(list: List[Variable], kind: VariableType): Boolean = {
      list.exists(_.kind == kind)
   }
}

