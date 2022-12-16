package base

import org.scalatest.funsuite.AnyFunSuite

class AdjacencyMatrixTest extends AnyFunSuite{
   // creates a matrix with dimension = 5 and content
   // given by
   // X1 -> X3
   // X2 -> X3
   // X3 -> X4
   // X3 -> X5
   // X5 -> X4

   // the content of the matrix must be the following one
   val content = Array[Int](0, 0, 1, 0, 0,
                            0, 0, 1, 0, 0,
                            0, 0, 0, 1, 1,
                            0, 0, 0, 0, 0,
                            0, 0, 0, 1, 0)

   // creates matrix object
   val matrix = AdjacencyMatrix(5, content)

   // gets the powers of matrix
   val powers: List[(Int, AdjacencyMatrix)] = matrix.computePowers

   /**
    * test the content for power 2
    */
   test("power 2 matrix"){
      val power2 = powers.find(pair => pair._1 == 2).get

      // the content of power two must be
      val patternArray = Array[Int](0, 0, 0, 1, 1,
                                        0, 0, 0, 1, 1,
                                        0, 0, 0, 1, 0,
                                        0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0)
      val patternMatrix = AdjacencyMatrix(5, patternArray)
      assert(power2._2 == patternMatrix)
   }

   /**
    * test the content of power 3
    */
   test("power 3 matrix"){
      val power3 = powers.find(pair => pair._1 == 3).get

      // the content of power two must be
      val patternArray = Array[Int](0, 0, 0, 1, 0,
         0, 0, 0, 1, 0,
         0, 0, 0, 0, 0,
         0, 0, 0, 0, 0,
         0, 0, 0, 0, 0)
      val patternMatrix = AdjacencyMatrix(5, patternArray)
      assert(power3._2 == patternMatrix)
   }

   /**
    * test the content of power 4
    */
   test("power 4 matrix"){
      val power4 = powers.find(pair => pair._1 == 4).get

      // the content of power two must be
      val patternArray = Array[Int](0, 0, 0, 0, 0,
         0, 0, 0, 0, 0,
         0, 0, 0, 0, 0,
         0, 0, 0, 0, 0,
         0, 0, 0, 0, 0)
      val patternMatrix = AdjacencyMatrix(5, patternArray)
      assert(power4._2 == patternMatrix)
   }

   /**
    * test the content of power 5
    */
   test("power 5 matrix"){
      val power5 = powers.find(pair => pair._1 == 5).get

      // the content of power two must be
      val patternArray = Array[Int](0, 0, 0, 0, 0,
         0, 0, 0, 0, 0,
         0, 0, 0, 0, 0,
         0, 0, 0, 0, 0,
         0, 0, 0, 0, 0)
      val patternMatrix = AdjacencyMatrix(5, patternArray)
      assert(power5._2 == patternMatrix)
   }
}
