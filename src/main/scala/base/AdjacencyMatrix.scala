package base

import base.AdjacencyMatrix.fromIndicesToOffset

import scala.annotation.{tailrec, targetName}

/**
 * class for representing adjacency matrices
 *
 * @param dimension dimension of the matrix
 * @param matrix    array with one values for links
 */
class AdjacencyMatrix(val dimension: Int, val matrix: Array[Int]) {
   /**
    * compute the powers of the matrix
    *
    * @return
    */
   def computePowers: List[(Int, AdjacencyMatrix)] = {
      @tailrec
      def go(counter: Int, powers: List[(Int, AdjacencyMatrix)]): List[(Int, AdjacencyMatrix)] = {
         if (counter > dimension) powers
         else {
            // get the last power
            val last = powers.head

            // keeps on multiplying
            go(counter + 1, (counter, last._2 * this) :: powers)
         }
      }

      go(3, List((2, this * this)))
   }

   /**
    * checks if there is a single path between two variables
    *
    * @param row target row for the check
    * @param column target column for the check
    * @return
    */
   def singlePath(row: Int, column: Int): Boolean = {
      if (!one(row, column)) false
      else {
         // compute powers
         val matrices = computePowers

         // in oll of them the position must be 0
         !matrices.exists(pair => pair._2.one(row, column))
      }
   }

   /**
    * checks if a given position contains a 1
    *
    * @param row target row for the check
    * @param column target column for the check
    * @return
    */
   def one(row: Int, column: Int): Boolean = {
      matrix(fromIndicesToOffset(dimension, row, column)) == 1
   }

   /**
    * toString method
    *
    * @return
    */
   override def toString: String = {
      var output: String = ""

      // loop over rows and cols
      for (i <- 0 until dimension) {
         for (j <- 0 until dimension) {
            output += s"$matrix(fromIndicesToOffset(dimension, i, j))" + " "
         }
         output += "\n"
      }

      // just return output
      output
   }

   /**
    * adds a new 1 to the matrix
    *
    * @param row target row
    * @param column target column
    * @return
    */
   @targetName("matrix addition")
   def +(row: Int, column: Int): AdjacencyMatrix = {
      // creates a new matrix
      val newMatrix = new Array[Int](dimension * dimension)

      // copy ones from this
      newMatrix ++ matrix

      // add the new one
      newMatrix(AdjacencyMatrix.fromIndicesToOffset(dimension, row, column)) = 1

      // creates a new Matrix
      AdjacencyMatrix(dimension, newMatrix)
   }

   /**
    * multiplication of matrices
    *
    * @param other matrix to multiply
    * @return
    */
   @targetName("matrix multiplication")
   def *(other: AdjacencyMatrix): AdjacencyMatrix = {
      val result = new Array[Int](dimension * dimension)

      // makes the product
      for (i <- 0 until dimension;
           j <- 0 until dimension;
           k <- 0 until dimension) {
         val resultIndex = fromIndicesToOffset(dimension, i, j)
         val thisIndex = fromIndicesToOffset(dimension, i, k)
         val otherIndex = fromIndicesToOffset(dimension, k, j)
         result(resultIndex) += matrix(thisIndex) * other.matrix(otherIndex)
      }

      // makes a new matrix
      AdjacencyMatrix(dimension, result)
   }

   /**
    * checks matrices equality
    *
    * @param other target variable to compare with
    * @return
    */
   @targetName("equality")
   def ==(other: AdjacencyMatrix): Boolean = {
      if (dimension != other.dimension) false
      else {
         // zip pair of values
         val distinctPair = matrix.zip(other.matrix).find(pair => pair._1 != pair._2)
         if (distinctPair.nonEmpty) false
         else true
      }
   }
}

/**
 * companion object
 */
object AdjacencyMatrix {
   /**
    * factory method
    *
    * @param dimension dimension of the matrix
    * @param ones      list of ones to add to the matrix
    * @return
    */
   def apply(dimension: Int, ones: List[(Int, Int)]): AdjacencyMatrix = {
      // add ones to the array
      val array = new Array[Int](dimension * dimension)

      // sets all the values
      ones.foreach(one => array(fromIndicesToOffset(dimension, one._1, one._2)) = 1)
      new AdjacencyMatrix(dimension, array)
   }

   /**
    * factory method with array of values
    *
    * @param dimension dimension of the matrix
    * @param ones      array of ones for the matrix
    */
   def apply(dimension: Int, ones: Array[Int]): AdjacencyMatrix = {
      new AdjacencyMatrix(dimension, ones)
   }

   /**
    * convert indices of row and column into an offset
    *
    * @param dimension dimension for converting indices into offset
    * @param row       target row
    * @param column       target column
    * @return
    */
   def fromIndicesToOffset(dimension: Int, row: Int, column: Int): Int = {
      row * dimension + column
   }
}

