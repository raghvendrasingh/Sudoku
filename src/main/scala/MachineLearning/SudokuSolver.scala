package MachineLearning

import scala.collection.mutable.ListBuffer
import scala.util.control._

/**
  * Created by raghvendra on 2/14/16.
  */
class SudokuSolver {
  /** This variable contains all the possible values in each cell of the matrix */
  val sudokuMatrix = ListBuffer.fill(9)(ListBuffer.fill(9)(ListBuffer[Int]()))
  /** This variable contains all the elements which are present inside input matrix.*/
  var matrixFormatted = ListBuffer.fill(9)(ListBuffer.fill(9)(ListBuffer[Int]()))

  /** This method checks whether a num is present inside given row of a matrix
    *
    * @param matrix - A ListBuffer[ListBuffer[ListBuffer[Int] ] ]
    * @param row - Row of a given matrix
    * @param col - Column of a given matrix
    * @param num - A number to be checked inside row of the matrix
    * @return - Return true if the num is found in row of the matrix otherwise false.
    */
  private def presentInRow(matrix: ListBuffer[ListBuffer[ListBuffer[Int]]], row: Int, col: Int, num: Int): Boolean = {
      var flag = false
      val loop = new Breaks
      loop.breakable {
        for (i <- matrix(row).indices if i != col) {
          if (matrix(row)(i).contains(num)) {
            flag = true
            loop.break()
          }
        }
      }
      flag
    }

  /** This method checks whether a num is present inside given col of a matrix
    *
    * @param matrix - A ListBuffer[ListBuffer[ListBuffer[Int] ] ]
    * @param row - Row of a given matrix
    * @param col - Column of a given matrix
    * @param num - A number to be checked inside col of the matrix
    * @return - Return true if the num is found in col of the matrix otherwise false.
    */
  private def presentInColumn(matrix: ListBuffer[ListBuffer[ListBuffer[Int]]], row: Int, col: Int, num: Int): Boolean = {
    var flag = false
    val loop = new Breaks
    loop.breakable {
      for (i <- matrix.indices if i != row) {
        if (matrix(i)(col).contains(num)) {
          flag = true
          loop.break()
        }
      }
    }
    flag
  }

  /** This method finds the top left cell coordinates of a 3X3 block inside a 9X9 matrix
    * For example if row = 3 and col = 5 then the result returned will be (3,3)
    * @param row - row index of the matrix
    * @param col - col index of the matrix
    * @return - Return a tuple containing coordinates of the top left cell of the 3X3 block of the 9X9 matrix
    */
  private def findBlockIndex(row: Int, col: Int): (Int,Int) = {
    var r = 0
    var c = 0
    val loop = new Breaks
    loop.breakable {
      for (i <- 0 to 3) {
        if (row - 3 * i < 0) {
          r = (i - 1)*3
          loop.break
        }
      }
    }

    loop.breakable {
      for (j <- 0 to 3) {
        if (col - 3 * j < 0) {
          c = (j - 1)*3
          loop.break
        }
      }
    }
    (r,c)
  }

  /** This method checks whether the num is present inside the block of tuple coordinate (row, col)
    *
    * @param matrix - A ListBuffer[ListBuffer[ListBuffer[Int] ] ]
    * @param row - row index of the matrix
    * @param col - col index of the matrix
    * @param num - A number to be checked inside col of the matrix
    * @return - Return true if the num is found inside block in which tuple coordinate (row,col) lies otherwise false
    */
  private def presentInBlock(matrix: ListBuffer[ListBuffer[ListBuffer[Int]]],row: Int, col: Int, num: Int): Boolean = {
    var flag = false
    val bi = findBlockIndex(row,col)
    val loop = new Breaks
    loop.breakable {
      for (i <- bi._1 to bi._1 + 2) {
        for (j <- bi._2 to bi._2 + 2) {
          if (matrix(i)(j).contains(num) && (i!=row || j!=col)) {
            flag = true
            loop.break()
          }
        }
      }
    }
    flag
  }

  /** This method creates a ListBuffer[ListBuffer[ListBuffer[Int]]] from  Vector[Vector[Int] ]
    *
    * @param matrix - A matrix in the form  Vector[Vector[Int] ]
    * @return - Returns a formatted matrix in the form ListBuffer[ListBuffer[ListBuffer[Int] ] ]
    */
  private def createFormattedMatrix(matrix: Vector[Vector[Int]]): ListBuffer[ListBuffer[ListBuffer[Int]]] = {
    val result = ListBuffer.fill(9)(ListBuffer.fill(9)(ListBuffer[Int]()))
    for (i <- matrix.indices) {
      for (j <- matrix(0).indices) {
        if (matrix(i)(j) != -1) result(i)(j) = result(i)(j) :+ matrix(i)(j)
       }
      }
    result
  }

  /** This method initializes sudokuMatrix. sudokuMatrix is in the format ListBuffer[ListBuffer[ListBuffer[Int]]]
    * Each cell of sudokuMatrix is a list containing all the elements which can come at that cell position of the matrix
    *
    * @param matrixOriginal - Input matrix in the form Vector[Vector[Int] ]
    */
  def initializeSudokuMatrix(matrixOriginal: Vector[Vector[Int]]): Unit = {
      matrixFormatted = createFormattedMatrix(matrixOriginal)
      for (i <- matrixFormatted.indices) {
        for (j <- matrixFormatted(0).indices) {
          if (matrixFormatted(i)(j).nonEmpty) sudokuMatrix(i)(j) = sudokuMatrix(i)(j) :+ matrixFormatted(i)(j)(0)
          else {
            for (k <- 1 to 9) {
              if (!presentInRow(matrixFormatted,i,j,k) && !presentInColumn(matrixFormatted,i,j,k) && !presentInBlock(matrixFormatted,i,j,k))
                sudokuMatrix(i)(j) = sudokuMatrix(i)(j) :+ k
            }
          }
        }
      }
    }

  /** This method prints the matrix
    *
    * @param matrix - Matrix to be printed
    */
  private def printMatrix(matrix: ListBuffer[ListBuffer[ListBuffer[Int]]]): Unit = {
    for(i <- matrix.indices) {
      for (j <- matrix(0).indices) {
        print(matrix(i)(j)(0) +" ")
      }
      println()
    }
  }

  /** This method makes a deep copy of the passed matrix in the form ListBuffer[ListBuffer[ListBuffer[Int]]]
    *
    * @param matrix - Matrix in the form ListBuffer[ListBuffer[ListBuffer[Int] ] ] which is to be copied
    * @return - Return a deep copy of passed matrix
    */
  private def copyMatrix(matrix: ListBuffer[ListBuffer[ListBuffer[Int]]]): ListBuffer[ListBuffer[ListBuffer[Int]]] = {
    val result = ListBuffer.fill(9)(ListBuffer.fill(9)(ListBuffer[Int]()))
    for (i <- matrix.indices) {
      for (j <- matrix(0).indices) {
        for (k <- matrix(i)(j)) {
          result(i)(j) = result(i)(j) :+ k
        }
      }
    }
    result
  }

  /** This method finds the solution of the sudoku
    *
    * @param runningMatrix - A runningMatrix which will finally contains the solution.
    *                      Matrix is filled in row major fashion
    * @param row - current position row index
    * @param col - current position col index
    */
  def solve(runningMatrix: ListBuffer[ListBuffer[ListBuffer[Int]]], row: Int, col: Int): Unit = {
    var i = row
    var j = col
    if (i>=8 && j>8){
      printMatrix(runningMatrix)
      System.exit(0)
    }
    if (j > 8){
      i = i + 1
      j = 0
    }
    if (i < 9 && j < 9) {
      for (k <- sudokuMatrix(i)(j)) {
        if (!presentInRow(runningMatrix, i, j, k) && !presentInColumn(runningMatrix, i, j, k) && !presentInBlock(runningMatrix,i, j, k)) {
          val nextMatrix = copyMatrix(runningMatrix)
          nextMatrix(i)(j).clear()
          nextMatrix(i)(j) = nextMatrix(i)(j) :+ k
          solve(nextMatrix, i, j + 1)
        }
      }
    }
  }

}

object ExecuteSudokuSolver extends App {
  /*val obj1 = new SudokuSolver
  val matrix1 = Vector(Vector(-1,4,-1,-1,-1,2,-1,8,-1),Vector(1,-1,-1,9,3,-1,-1,-1,2),
    Vector(-1,-1,-1,-1,-1,6,-1,-1,-1),Vector(-1,-1,5,-1,7,-1,9,-1,-1),
    Vector(-1,1,2,-1,-1,-1,6,4,-1),Vector(-1,-1,8,-1,5,-1,7,-1,-1),
    Vector(-1,-1,-1,4,-1,-1,-1,-1,-1),Vector(3,-1,-1,-1,6,8,-1,-1,7),
    Vector(-1,9,-1,1,-1,-1,-1,3,-1))
  obj1.initializeSudokuMatrix(matrix1)
  obj1.solve(obj1.matrixFormatted, 0, 0)

  val obj2 = new SudokuSolver
  val matrix2 = Vector(Vector(2,-1,-1,6,-1,3,-1,-1,-1),Vector(-1,-1,-1,-1,1,-1,-1,7,8),
    Vector(-1,-1,1,-1,-1,-1,-1,-1,4),Vector(-1,6,-1,-1,-1,-1,2,5,-1),
    Vector(-1,4,-1,-1,7,-1,-1,6,-1), Vector(-1,5,8,-1,-1,-1,-1,1,-1),
    Vector(3,-1,-1,-1,-1,-1,5,-1,-1), Vector(6,2,-1,-1,4,-1,-1,-1,-1),
    Vector(-1,-1,-1,7,-1,8,-1,-1,2))
  obj2.initializeSudokuMatrix(matrix2)
  obj2.solve(obj2.matrixFormatted, 0, 0)*/

  val obj3 = new SudokuSolver
  val matrix3 = Vector(Vector(5,-1,-1,2,-1,-1,8,-1,-1),Vector(-1,3,-1,7,-1,-1,-1,-1,5),
    Vector(4,-1,1,-1,-1,-1,-1,-1,-1),Vector(-1,-1,-1,6,-1,-1,4,-1,-1),
    Vector(-1,-1,2,-1,5,-1,3,-1,-1), Vector(-1,-1,7,-1,-1,9,-1,-1,-1),
    Vector(-1,-1,-1,-1,-1,-1,9,-1,1), Vector(6,-1,-1,-1,-1,4,-1,2,-1),
    Vector(-1,-1,9,-1,-1,3,-1,-1,7))
  obj3.initializeSudokuMatrix(matrix3)
  obj3.solve(obj3.matrixFormatted, 0, 0)
}
