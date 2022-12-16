package search.base

import auxiliar.Action

/**
 * class for storing solutions
 * @param actions list of actions composing the solution of 
 *                the problem
 */
case class Solution(actions : List[Action]){
   val cost: Double = actions.map(_.cost).max
}
