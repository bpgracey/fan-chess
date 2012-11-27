/**
 *
 */
package com.bpgracey.chessdemo

import scala.collection.mutable.HashMap

/**
 * Pieces
 * color: true if White (going 'up' the board), false if Black
 */
sealed abstract class Piece(val color: Boolean) {
  val otherColor = !color // for convenience! - this could also be a def or lazy val, to save unnecessary calculation
  def listMovesFrom(square: Square): List[Square] // list of squares piece could move to from given square - may need to be List[(Square, Piece)] to cover promotions
  def listTakesFrom(square: Square): List[(Square, Square)] // list of tuples: square to move to, square to remove piece from - see above, may need to be List[(Square, Square, Piece)]
  def sameColorAs(xcolor: Boolean): Boolean = xcolor == color
  def oppositeColorTo(xcolor: Boolean): Boolean = xcolor != color
  val NoMoves = List()
}

/**
 * Pseudo piece to represent no piece at all
 */
object NoPiece extends Piece(true) {
  override def listMovesFrom(square: Square): List[Square] = List()
  override def listTakesFrom(square: Square): List[(Square, Square)] = List()
  override def sameColorAs(xcolor: Boolean) = false
  override def oppositeColorTo(xcolor: Boolean) = false
  override def toString = ""
}

case class Pawn(override val color: Boolean) extends Piece(color) {
  val direction = if (color) 1 else -1
  
  override def listMovesFrom(square: Square): List[Square] = {
    // basic pawn move: 1 ahead
    // TODO: on start square, can move 2 ahead if not blocked
    // TODO: on moving to last rank, pawn can promote to Rook, Knight, Bishop or Queen (may need to rethink move & take...)
    val possibleMoves = List(square.dy(direction)) // all legal moves (maybe make this a Stream for, eg, bishops & rooks?)
    val validMoves = possibleMoves filter (p => p isValid) // ... less all moves moving off edge of board...
    val availableMoves = validMoves filterNot (p => p.isOccupied) // ... less all moves onto occupied squares
    return availableMoves 
  }
  
  override def listTakesFrom(square: Square): List[(Square, Square)] = {
    // basic pawn take: one diagonally ahead to left or right
    // TODO: on start square, can take 'en passant' - pawn goes 2 ahead, takes piece on left or right
    // TODO: on moving to last rank, pawn can promote to Rook, Knight, Bishop or Queen (may need to rethink move & take...)
    val pos = square.dy(direction)
    val possibles = List(pos.dx(1), pos.dx(-1)) // all legal takes
    val validTakes = possibles filter(x => x isValid) // less all takes off edge of board (technically not necessary)
    val availableTakes = validTakes filter (x => x isOccupiedBy otherColor) // less all takes not occupied by enemy
    return for {x <- availableTakes} yield (x, x) // and convert to tuples
  }
  
  override def toString = "P"
}

object WhitePawn extends Pawn(true)  { override def toString = "wP"}
object BlackPawn extends Pawn(false) { override def toString = "bP"}

/**
 * A square on the board
 */
case class Square(x: Int, y: Int) {
  def dx(d: Int) = copy(x = x + d, y) // move in x-direction by d
  def dy(d: Int) = copy(x, y = y + d) // move in y-direction by d
  def isValid = Board.isOnBoard(this)
  def piece: Piece = Board pieceAt this
  def isOccupied = piece != NoPiece
  def isOccupiedBy(color: Boolean): Boolean = piece sameColorAs color
  def listMoves: List[Square] = piece listMovesFrom this
  def listTakes: List[(Square, Square)] = piece listTakesFrom this
  override def toString = piece.toString + ('a'+x).toChar + (y + 1)
}

/**
 * The chessboard
 * Essentially a wrapper around a mutable HashMap of occupied squares
 */
object Board {
  /**
   * Check if our square is actually on the board...
   * (by keeping the board valid function, isOnBoard, here, we allow for future non-square boards, eg. Vulcan chess...)
   */
  def isOnBoard(p: Square): Boolean = p.x >= 0 && p.x < 8 && p.y >= 0 && p.y < 8
  
  val pieces = HashMap[Square, Piece]()
  
  // for setting out the board
  def place(square: Square, piece: Piece): Unit = pieces += ((square, piece))
  
  def pieceAt(square: Square): Piece = pieces.getOrElse(square, NoPiece)
  
  def move(from:Square, to:Square): Unit = {
    if (from.isOccupied && !to.isOccupied)
    	pieces += ((to, pieces.remove(from).get))
  }
  
  def take(from: Square): Unit = pieces -= from
  
  def clear(): Unit = pieces.clear
  
  override def toString = pieces.keySet.foldLeft("")((b, a) => b + a + ",") // quick & dirty representation of board state
}
