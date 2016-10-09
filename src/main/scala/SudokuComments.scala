/*
:paste
import Solution._, Boards._
val p = "....8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"
val s1 = "145289376269371584837564219976125438513498627482736951391657842728943165654812793"
val s2 = "245981376169273584837564219976125438513498627482736951391657842728349165654812793"
val puzzle = parse(p)


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


===============================================================================================================

// def parseAlt(str: String): Board = {
//     val startBoard = new Board(emptyBoard)
//     str.reverse.foldRight((startBoard,0))((item, curr) => item match {
//         case '.' => {
//           val (row,col) = (curr._2/9, curr._2%9) 
//           if(debug) println("Available Values At " + (row,col) + ": " + curr._1.availableValuesAt(row,col))
//           (curr._1, curr._2+1) // if '.' leave default available spots in there
//         } 
//         case num => {
//           val (row,col) = (curr._2/9, curr._2%9) 
//           val currPeers = peers(row,col)
//           val newAvailable = curr._1.available.map{case (pos,lst) => {
//               if(pos == (row,col)) // if available is at the current spot, 
//                 (pos, List(num.toInt - 48)) // return the position mapped to List(num)
//               else if(currPeers.contains(pos)) // if available exists in peers of current spot
//                 (pos, lst.filterNot(x => x==num.toInt - 48)) // take num out of peers
//               else
//                 (pos, lst)
//             }
//           }
//           if(debug) println("Available Values At " + (row,col) + ": " + curr._1.availableValuesAt(row,col))
//           (new Board(newAvailable), curr._2 + 1)
//         }
//       }
//     )._1 //returns original board with smaller available spots list
//   }

===============================================================================================================
def place(row: Int, col: Int, value: Int): Board = {
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

==============================================================================================================

  def getNewAvailable(row: Int, col: Int, value: Int): Map[(Int,Int), List[Int]] = {
    val peersList = Solution.peers(row,col)
    available.map{case (pos,lst) => {
      if (pos == (row,col)) // if pos is the spot trying to place, change the list to be List(value)
        (pos, List(value))
      else if (peersList.contains(pos)){
        val newAvailCell =  (pos, lst.filterNot(x => x == value)) // take out 'value' from possible peers
        if(available(pos).filterNot(x => x == value).length == 1) 
        // if available at pos with the filtered list only has one element left
          new Board(available + (newAvailCell)).getNewAvailable(
                    pos._1, pos._2, available(pos).filterNot(x => x == value)(0))
        
        newAvailCell
      }
      else // if pos isn't equal to anything we care about, continue checking
        (pos, lst)
      }
    }
  }

  =============================================================================================================

def solve(): Option[Board] = isSolved match {
    case true => Some(new Board(available))
    case false => {
      //val s = Stack[Int]()
      solveHelper(this.nextStates)
    }
  }

  def solveHelper(boards: List[Board]): Option[Board] = boards match {
    case Nil => print("0,  "); None
    case b :: rest => {
      print(boards.length + ",  ")
      if(b.isSolved)
        Some(b)
      else{
        if(solveHelper(b.nextStates) == None)
          solveHelper(rest)
        else
          solveHelper(b.nextStates ::: rest)
      }
    } 
  }

=======================================================================================================================

 def solveHelper(boards: Stack[Board]): Option[Board] = {
    if(boards.isEmpty) 
      None
    else {
      print(boards.length + ",  ")
      val b = boards.pop
      if(b.isSolved)
        Some(b)
      else{
        b.nextStates.foreach(x => boards.push(x))
        solveHelper(boards)
      }
    } 
  }

=======================================================================================================================

def solve(): Option[Board] = isSolved match {
    case true => Some(new Board(available))
    case false => {
      val s = Stack[Board]()
      this.nextStates.reverse.foreach(x => s.push(x))
      solveHelper(s)
    }
  }

  def solveHelper(boards: Stack[Board]): Option[Board] = {
    if(boards.isEmpty) 
      None
    else {
      val b = boards.pop
      if(b.isSolved)
        Some(b)
      else{
        //println("Adding States...")
        b.nextStates.reverse.foreach(x => boards.push(x))
        solveHelper(boards)
      }
    } 
  }

=======================================================================================================================
 
  // def nextStates(): List[Board] = {
  //   if (isUnsolvable() || isSolved()) { // if it's unsolvable or already solved, nothing to return
  //     List()
  //   }
  //   val positions: List[(Int,Int)] = available.keys.toList.filterNot(x => available(x).length == 1).sorted 
  //   // returns a sorted list of ((0,0) ... (8,8))
  //   val boards = positions.map(x => available(x).map(y => place(x._1, x._2, y))).flatten
  //   boards.filterNot(x => x.isUnsolvable).sortWith((x,y) => 
  //           getNumAvail(x.available) < getNumAvail(y.available))
  // }

=============================================================================================================================
    // def solveHelper(boards: Queue[Board]): Option[Board] = {
  //   if(boards.isEmpty) 
  //     None
  //   else {
  //     val b = boards.dequeue
  //     if(b.isSolved)
  //       Some(b)
  //     else{
  //       //println("Adding States...")
  //       b.nextStates.foreach(x => boards.enqueue(x))
  //       solveHelper(boards)
  //     }
  //   } 
  // }


  */