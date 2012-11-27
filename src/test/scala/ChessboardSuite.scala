import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import com.bpgracey.chessdemo._

@RunWith(classOf[JUnitRunner])
class ChessboardSuite extends FunSuite {
  
  
	test("valid squares") {
	  val sq1 = new Square(1, 1)
	  val sq2 = new Square(5, 7)
	  val sq3 = new Square(-1, 11)
	  val sq4 = new Square(7, -11)
	  assert(sq1 isValid, "1, 1")
	  assert(sq2 isValid, "5, 7")
	  assert(!(sq3 isValid), "-1, 11")
	  assert(!(sq4 isValid), "7, -11")
	}
	
	test("moving white") {
	  Board clear
	  val startPos = new Square(4, 6)
	  Board place(startPos, WhitePawn)
	  
	  val expectedPos = new Square(4, 7)
	  val moves = WhitePawn listMovesFrom startPos
	  assert(moves == List(expectedPos), "Move")
	  
	  Board move(startPos, moves(0))
	  assert(Board.pieceAt(startPos) == NoPiece, "Pawn moved on")
	  assert(Board.pieceAt(expectedPos) == WhitePawn, "Pawn arrived")
	  
	  assert(WhitePawn.listMovesFrom(expectedPos) == List(), "Edge of board")
	}
	
	test("moving black") {
	  Board clear
	  val startPos = new Square(5, 1)
	  Board place(startPos, BlackPawn)
	  
	  val expectedPos = new Square(5, 0)
	  val moves = BlackPawn listMovesFrom startPos
	  assert(moves == List(expectedPos), "Move")
	  
	  Board move(startPos, moves(0))
	  assert(Board.pieceAt(startPos) == NoPiece, "Pawn moved on")
	  assert(Board.pieceAt(expectedPos) == BlackPawn, "Pawn arrived")
	  
	  assert(BlackPawn.listMovesFrom(expectedPos) == List(), "Edge of board")
	}
	
	test("collision") {
	  Board clear

	  val sq1 = new Square(4, 4)
	  val sq2 = new Square(4, 5)
	  val sq3 = new Square(4, 6)
	  
	  Board place (sq1, WhitePawn)
	  Board place (sq2, WhitePawn)
	  Board place (sq3, BlackPawn)
	  
	  assert(sq1.listMoves == List(), "white-white")
	  assert(sq2.listMoves == List(), "white-black")
	  assert(sq3.listMoves == List(), "black-white")
	}
	
	test("take") {
	  Board clear
	  
	  val sq1 = new Square(4, 4)
	  val sq2 = new Square(3, 5)
	  
	  Board place (sq1, WhitePawn)
	  assert(WhitePawn.listTakesFrom(sq1) == List(), "No takes")
	  assert(sq1.listTakes == List(), "No takes (2)")
	  
	  Board place (sq2, BlackPawn)
	  
	  val takes = sq1 listTakes
	  
	  assert(takes == List((sq2, sq2)), "Only one take")
	  
	  // the takedown
	  Board take sq2
	  assert(Board.pieceAt(sq2) == NoPiece, "Black pawn removed")
	  Board move(sq1, sq2)
	  assert(Board.pieceAt(sq2) == WhitePawn, "Pawn taken")
	}
}