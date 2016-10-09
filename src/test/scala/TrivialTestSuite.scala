class TrivialTestSuite extends org . scalatest . FunSuite {
	import Solution._
	import Boards._

	val puzzle = "....8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"

	test ("The solution object must be defined") {
		val obj: hw.sudoku.SudokuLike = Solution
	}

	test ("peers test") {
		val actualOutput = peers(0,0).toSet
		val expectedOutput = 
			List((0,1), (0,2), (0,3), (0,4), (0,5), (0,6), (0,7), (0,8), 
				 (1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0), (8,0), 
				 (1,1), (1,2), (2,1), (2,2)).toSet
		assert(expectedOutput == actualOutput)
	}

	test ("parse compiles properly") {
		parse(puzzle)
	}

	test ("valueAt test") {
		val b = parse(puzzle)
		assert(b.valueAt(0,0) == None)
		assert(b.valueAt(0,4) == Some(8))
		assert(b.valueAt(2,4) == Some(6))
		assert(b.valueAt(2,7) == None)
	}

	test ("isSolved test") {
		val solvablePuzzle = "145289376269371584837564219976125438513498627482736951391657842728943165654812793"
		val solved = parse(solvablePuzzle)
		assert(solved.isSolved == true)
		assert(parse(puzzle).isSolved == false)
	}

	test ("place test") {
		val b = parse(puzzle)
		assert(b.valueAt(1,6) == None) // No value at (1,6) before calling place
		val placed = b.place(1,6,5)
		assert(placed.valueAt(1,6) == Some(5)) // Placed a 5
		assert(placed.valueAt(1,2) == Some(9)) // 9 was only available left at that cell
		val cell_1_6_Peers = peers(1,6)
		val cell_1_2_Peers = peers(1,2)
		assert(cell_1_6_Peers.forall(pos => placed.available(pos).contains(5) == false)) // no 5's in any peers
		assert(cell_1_2_Peers.forall(pos => placed.available(pos).contains(9) == false)) // no 9's in any peers
	}

	test ("solve tests - 10 of Norvig's 50 Easy") {
		assert(deParse(parse(p1).solve.get) == 
			"483921657967345821251876493548132976729564138136798245372689514814253769695417382" )
		assert(deParse(parse(p3).solve.get) == 
			"462831957795426183381795426173984265659312748248567319926178534834259671517643892" )
		assert(deParse(parse(p21).solve.get) == 
			"428531796365947182971268435214896573697453218583172964849615327752389641136724859" )
		assert(deParse(parse(p27).solve.get) == 
			"387256419469781325512439867123548976758963241694127583835674192271895634946312758" )
		assert(deParse(parse(p32).solve.get) == 
			"132649785758213649964785123543897216276531894891426537619378452327154968485962371" )
		assert(deParse(parse(p35).solve.get) == 
			"453218796629753481178496532796582314314967825285134679542879163937641258861325947" )
		assert(deParse(parse(p41).solve.get) == 
			"814976532659123478732854169948265317275341896163798245391682754587439621426517983" )
		assert(deParse(parse(puzzle).solve.get) == 
			"145289376269371584837564219976125438513498627482736951391657842728943165654812793" )
		assert(deParse(parse(p45).solve.get) == 
			"586127943723469851491853267135974628279618534648532179917246385352781496864395712" )
		assert(deParse(parse(p50).solve.get) == 
			"351286497492157638786934512275469183938521764614873259829645371163792845547318926" )
	}

}