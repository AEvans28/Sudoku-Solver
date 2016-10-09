import hw.sudoku._
import scala.collection.mutable.Queue

object Solution extends SudokuLike {
  type T = Board

// Modified from Discussion 3/23/2016
  val emptyBoard = Map((0.to(8).flatMap{r =>
    0.to(8).map{ 
      c => ((r,c) -> 1.to(9).toList) 
    }
  }):_*)

// Given from Discussion 3/23/2016  
  def parse(str: String): Board = {
    val startBoard = new Board(emptyBoard)
    str.reverse.foldRight((startBoard,0))((item, curr) => item match {
        case '.' => (curr._1, curr._2 + 1)
        case num => {
          val (row,col) = (curr._2/9, curr._2%9)
          (curr._1.place(row,col,num.toInt-48), curr._2 + 1)
        }
      }
    )._1
  }

// Given from Discussion 3/23/2016
  def calcPeers(row: Int, col: Int): List[(Int, Int)] = {
    val rowPeers = 0.to(8).map(r => (r,col))
    val colPeers = 0.to(8).map(c => (row,c))
    val boxRow: Int = (row/3)*3
    val boxCol: Int = (col/3)*3
    val boxPeers = boxRow.to(boxRow+2).flatMap(r => {
      boxCol.to(boxCol+2).map(c => (r,c))
    })
    (rowPeers++colPeers++boxPeers).filterNot{
      case (r,c) => r == row && c == col
    }.toList
  }

// Given from Discussion 3/23/2016
  val peersTable = Map((0.to(8).flatMap{r =>
      0.to(8).map{c => 
        ((r,c) -> calcPeers(r,c))
      }
    }):_*)

  // You can use a Set instead of a List (or, any Iterable)
  def peers(row: Int, col: Int): List[(Int, Int)] = {
    peersTable((row,col))
  }

// Taken from PIAZZA --- I liked it 
  def time(f: => Unit) = {
    val s = System.currentTimeMillis
    f
    (System.currentTimeMillis - s) / 1000.00
  }

// Created for use in testing - converts a board to a string to compare with.
  def deParse(b: Board): String = {
    val keys = b.available.keys.toList.sorted
    deParseHelper(keys, "", b)
  }

// Helper for my deParse function
  def deParseHelper(cells: List[(Int,Int)], s: String, b: Board): String = cells match {
    case Nil => s+""
    case pos :: rest => {
      if(b.available(pos).length > 1){
        deParseHelper(rest, s+'.', b)
      }
      else{
        val character = b.available(pos)(0).toString
        deParseHelper(rest, s+character, b)
      } 
    }
  }

}

// Top-left corner is (0,0). Bottom-right corner is (8,8). Feel free to
// change the fields of this class.
class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {

  def availableValuesAt(row: Int, col: Int): List[Int] = {
    // Assumes that a missing value means all values are available. Feel
    // free to change this.
    available.getOrElse((row, col), 1.to(9).toList)
  }

  def valueAt(row: Int, col: Int): Option[Int] = {
    available(row,col) match {
      case x :: Nil => Some(x)
      case _ => None
    }
  }

  def isSolved(): Boolean = {
    available.forall(x => x._2.length == 1)
  } 

  def isUnsolvable(): Boolean = {
    val emptyCells = available.filter(x => x._2 == List()) // filter out any cells with no list
    !emptyCells.isEmpty // if emptyCells is NOT an empty map, it has unsolvable cells
  }

  def place(row: Int, col: Int, value: Int): Board = {
    require(availableValuesAt(row, col).contains(value))
    val filteredAvail = available + ((row,col) -> List(value))
    val newAvailable = seekAndDestroy(filteredAvail, Solution.peers(row, col).toList, value)
    new Board(newAvailable)
  }

/* 
CS 220 OFFICE HOURS -- BRENDAN MURHPY HELPED 
He gave helper method header and hinted at psuedo code, 
I did the rest while talking about it with him 
I left my attempts as comments at the bottom of the code. 
*/
  def seekAndDestroy(available: Map[(Int,Int),List[Int]],
                    positions: List[(Int,Int)],
                    value: Int): Map[(Int,Int), List[Int]] =
    positions match {
      case Nil => available
      case p :: rest => {
        //println("Available: " + available)
        //println("P: " + p)
        val availVals = available.getOrElse(p, 1.to(9).toList)
        if(availVals.contains(value)) {
          val filteredVals = availVals.filterNot(x => x == value)
          val updatedAvail = available + (p->filteredVals)
          val recurAvail = {
            if(filteredVals.length == 1)
              seekAndDestroy(updatedAvail, Solution.peers(p._1, p._2).toList, filteredVals.head)
            else
              updatedAvail
          }
          seekAndDestroy(recurAvail, rest, value)
      }
        else
          seekAndDestroy(available, rest, value)
      }
  }

  val positions = Solution.emptyBoard.keys.toList   // returns a sorted list of ((0,0) ... (8,8))
  //You can return any Iterable (e.g., Stream)
  def nextStates(): Stream[Board] = {
    if (isUnsolvable() || isSolved()) { // if it's unsolvable or already solved, nothing to return
      Stream[Board]()
    }
    val boards = positions.filter(x => available(x).length != 1).map(pos => 
          available(pos).map(elem => place(pos._1, pos._2, elem))).flatten

    boards.filterNot(x => x.isUnsolvable).sortWith((x,y) => 
           getNumAvail(x.available) < getNumAvail(y.available)).toStream
  }

  def getNumAvail(available: Map[(Int, Int), List[Int]]): Int = {
    // minimum of 81 numbers
    available.values.toList.flatten.length
  }

def solve(): Option[Board] = isSolved match {
    case true => Some(new Board(available))
    case false => {
      val q = Queue[Stream[Board]](nextStates)
      solveHelper(q)
    }
  }

  def solveHelper(boards: Queue[Stream[Board]]): Option[Board] = {
    if(boards.isEmpty){ // No solution found
      //println("Oops, I broke it")
      None
    } else {
        val b = boards.dequeue
        if(b == Stream()){ // if b is an empty stream, end of path
            println("Current Stream Empty")
            solveHelper(boards)
         } else { // if b isn't empty, test the head, recurse on tail
          val head = b.head
          if(head.isSolved){ // if next board in stream is solved, return it
           println("Solved")
            Some(head)
          } else { // if next board is not solved, recurse on tail 
              val tail = b.tail
              if(tail == Stream()){ // if tail is empty, end of current path
                println("FINISHED")
                solveHelper(boards)
              } else { // if tail is not empty, enqueue the tail and head's next states, recurse  
                println("Continuing on Path")
                boards.enqueue(tail)
                boards.enqueue(head.nextStates)
                solveHelper(boards)
              }
            }
          } 
      }
  }

}

/*def place(row: Int, col: Int, value: Int): Board = {
    require(availableValuesAt(row, col).contains(value))
    val peersList = Solution.peers(row,col)
    val newAvailable = available.map{case (pos,lst) => {
      if (pos == (row,col)) 
        (pos, List(value))
      else if (peersList.contains(pos)) {
        val new_avails = lst.filterNot(x => x == value) // take out 'value' from possible peers
        (pos, new_avails)
      }
      else
        (pos, lst)
      }
    }

    val newnewAvailable = newAvailable.foldRight(new Board(newAvailable))( (item, base) => {
        if(item._2.length == 1) {
          val v = Solution.peers(item._1._1, item._1._2).flatMap{ pair => available(pair) }
          if(v.count(_==value) == 2) {
            print(v)
            base.place(item._1._1, item._1._2, item._2(0))  
          } else {
            base
          }
        } else {
          base
        }
      }
    )
    val a = newnewAvailable
    print(a + "\n")
    a
  }

===============================================================================================================

def place(row: Int, col: Int, value: Int): Board = {
    require(availableValuesAt(row, col).contains(value))
    val peersList = Solution.peers(row,col)
    val newAvailable = available.map{case (pos,lst) => {
      if (pos == (row,col)) 
        (pos, List(value))
      else if (peersList.contains(pos))
        (pos, lst.filterNot(x => x == value)) // take out 'value' from possible peers
      else
        (pos, lst)
      }
    }
    new Board(newAvailable)
  }
  */