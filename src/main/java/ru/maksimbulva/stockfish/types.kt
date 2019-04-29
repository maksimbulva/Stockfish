import com.sun.org.apache.xpath.internal.operations.Bool

/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2019 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

  Stockfish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Stockfish is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

// TODO
//typedef uint64_t Key;
//typedef uint64_t Bitboard;

val MAX_MOVES = 256
val MAX_PLY   = 128

/// A move needs 16 bits to be stored
///
/// bit  0- 5: destination square (from 0 to 63)
/// bit  6-11: origin square (from 0 to 63)
/// bit 12-13: promotion piece type - 2 (from KNIGHT-2 to QUEEN-2)
/// bit 14-15: special move flag: promotion (1), en passant (2), castling (3)
/// NOTE: EN-PASSANT bit is set only when a pawn can be captured
///
/// Special cases are MOVE_NONE and MOVE_NULL. We can sneak these in because in
/// any normal move destination square is always different from origin square
/// while MOVE_NONE and MOVE_NULL have the same origin and destination square.

inline class Move(val v: Int) {
  companion object {
    val MOVE_NONE = Move(0)
    val MOVE_NULL = Move(65)
  }
}

inline class MoveType(val v: Int) {
  companion object {
    val NORMAL = MoveType(0)
    val PROMOTION = MoveType(1 shl 14)
    val ENPASSANT = MoveType(2 shl 14)
    val CASTLING  = MoveType(3 shl 14)
  }
}

inline class Color(val v: Int) {
  companion object {
    val WHITE = Color(0)
    val BLACK = Color(1)
    val COLOR_NB = Color(2)
  }
}

inline class CastlingSide(val v: Int) {
  companion object {
    val KING_SIDE = CastlingSide(0)
    val QUEEN_SIDE = CastlingSide(1)
    val CASTLING_SIDE_NB = CastlingSide(2)
  }
}

inline class CastlingRight(val v: Int) {
  companion object {
    val NO_CASTLING = CastlingRight(0)
    val WHITE_OO = CastlingRight(1)
    val WHITE_OOO = CastlingRight(WHITE_OO.v shl 1)
    val BLACK_OO  = CastlingRight(WHITE_OO.v shl 2)
    val BLACK_OOO = CastlingRight(WHITE_OO.v shl 3)

    val WHITE_CASTLING = CastlingRight(WHITE_OO.v or WHITE_OOO.v)
    val BLACK_CASTLING = CastlingRight(BLACK_OO.v or BLACK_OOO.v)
    val ANY_CASTLING   = CastlingRight(WHITE_CASTLING.v or BLACK_CASTLING.v)

    val CASTLING_RIGHT_NB = CastlingRight(16)
  }
}

inline class Phase(val v: Int) {
  companion object {
    val PHASE_ENDGAME = Phase(0)
    val PHASE_MIDGAME = Phase(128)
    val MG = Phase(0)
    val EG = Phase(1)
    val PHASE_NB = Phase(2)
  }
}

inline class ScaleFactor(val v: Int) {
  companion object {
    val SCALE_FACTOR_DRAW    = ScaleFactor(0)
    val SCALE_FACTOR_NORMAL  = ScaleFactor(64)
    val SCALE_FACTOR_MAX     = ScaleFactor(128)
    val SCALE_FACTOR_NONE    = ScaleFactor(255)
  }
}

inline class Bound(val v: Int) {
  companion object {
    val BOUND_NONE = Bound(0)
    val BOUND_UPPER = Bound(1)
    val BOUND_LOWER = Bound(2)
    val BOUND_EXACT = Bound(BOUND_UPPER.v or BOUND_LOWER.v)
  }
}

inline class Value(val v: Int) {
  companion object {
    val VALUE_ZERO = Value(0)
    val VALUE_DRAW = Value(0)
    val VALUE_KNOWN_WIN = Value(10000)
    val VALUE_MATE = Value(32000)
    val VALUE_INFINITE = Value(32001)
    val VALUE_NONE = Value(32002)

    val VALUE_MATE_IN_MAX_PLY = Value(VALUE_MATE.v - 2 * MAX_PLY)
    val VALUE_MATED_IN_MAX_PLY = Value(-VALUE_MATE.v + 2 * MAX_PLY)

    val PawnValueMg = Value(128)
    val PawnValueEg = Value(213)
    val KnightValueMg = Value(782)
    val KnightValueEg = Value(865)
    val BishopValueMg = Value(830)
    val BishopValueEg = Value(918)
    val RookValueMg = Value(1289)
    val RookValueEg = Value(1378)
    val QueenValueMg = Value(2529)
    val QueenValueEg = Value(2687)

    val MidgameLimit = Value(15258)
    val EndgameLimit = Value(3915)
  }
}

inline class PieceType(val v: Int) {
  companion object {
    val NO_PIECE_TYPE = PieceType(0)
    val PAWN = PieceType(1)
    val KNIGHT = PieceType(2)
    val BISHOP = PieceType(3)
    val ROOK = PieceType(4)
    val QUEEN = PieceType(5)
    val KING = PieceType(6)
    val ALL_PIECES = PieceType(0)
    val PIECE_TYPE_NB = PieceType(8)
  }
}

inline class Piece(val v: Int) {
  companion object {
    val NO_PIECE = Piece(0)
    val W_PAWN = Piece(1)
    val W_KNIGHT = Piece(2)
    val W_BISHOP = Piece(3)
    val W_ROOK = Piece(4)
    val W_QUEEN = Piece(5)
    val W_KING = Piece(6)
    val B_PAWN = Piece(9)
    val B_KNIGHT = Piece(10)
    val B_BISHOP = Piece(11)
    val B_ROOK = Piece(12)
    val B_QUEEN = Piece(13)
    val B_KING = Piece(14)
    val PIECE_NB = Piece(16)
  }
}

// TODO
// extern Value PieceValue[PHASE_NB][PIECE_NB];

inline class Depth(val v: Int) {
  companion object {
    val ONE_PLY = Depth(1)

    val DEPTH_ZERO          =  Depth(0 * ONE_PLY.v)
    val DEPTH_QS_CHECKS     =  Depth(0 * ONE_PLY.v)
    val DEPTH_QS_NO_CHECKS  = Depth(-1 * ONE_PLY.v)
    val DEPTH_QS_RECAPTURES = Depth(-5 * ONE_PLY.v)

    val DEPTH_NONE = Depth(-6 * ONE_PLY.v)
    val DEPTH_MAX  = Depth(MAX_PLY * ONE_PLY.v)
  }
}

// TODO
// static_assert(!(ONE_PLY and (ONE_PLY - 1)), "ONE_PLY is not a power of 2");

inline class Square(val v: Int) {
  companion object {
    const val SQ_A1 = 0
    const val SQ_H8 = 63
  }
  // TODO
//  SQ_A1, SQ_B1, SQ_C1, SQ_D1, SQ_E1, SQ_F1, SQ_G1, SQ_H1,
//  SQ_A2, SQ_B2, SQ_C2, SQ_D2, SQ_E2, SQ_F2, SQ_G2, SQ_H2,
//  SQ_A3, SQ_B3, SQ_C3, SQ_D3, SQ_E3, SQ_F3, SQ_G3, SQ_H3,
//  SQ_A4, SQ_B4, SQ_C4, SQ_D4, SQ_E4, SQ_F4, SQ_G4, SQ_H4,
//  SQ_A5, SQ_B5, SQ_C5, SQ_D5, SQ_E5, SQ_F5, SQ_G5, SQ_H5,
//  SQ_A6, SQ_B6, SQ_C6, SQ_D6, SQ_E6, SQ_F6, SQ_G6, SQ_H6,
//  SQ_A7, SQ_B7, SQ_C7, SQ_D7, SQ_E7, SQ_F7, SQ_G7, SQ_H7,
//  SQ_A8, SQ_B8, SQ_C8, SQ_D8, SQ_E8, SQ_F8, SQ_G8, SQ_H8,
//  SQ_NONE,
//
//  SQUARE_NB = 64
}

inline class Direction(val v: Int) {
  companion object {
    val NORTH = Direction(8)
    val EAST = Direction(1)
    val SOUTH = Direction(-NORTH.v)
    val WEST = Direction(-EAST.v)

    val NORTH_EAST = Direction(NORTH.v + EAST.v)
    val SOUTH_EAST = Direction(SOUTH.v + EAST.v)
    val SOUTH_WEST = Direction(SOUTH.v + WEST.v)
    val NORTH_WEST = Direction(NORTH.v + WEST.v)
  }
}

inline class File(val v: Int) {
  // TODO
  // FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H, FILE_NB
}

inline class Rank(val v: Int) {
  companion object {
    const val RANK_1 = 0
    const val RANK_2 = 1
    const val RANK_3 = 2
    const val RANK_4 = 3
    const val RANK_5 = 4
    const val RANK_6 = 5
    const val RANK_7 = 6
    const val RANK_8 = 7
    const val RANK_NB = 8
  }
}


/// Score inline class (val v: Int) stores a middlegame and an endgame value in a single integer (inline class (val v: Int)).
/// The least significant 16 bits are used to store the middlegame value and the
/// upper 16 bits are used to store the endgame value. We have to take care to
/// avoid left-shifting a signed int to avoid undefined behavior.
inline class Score(val v: Int) {
  companion object {
    val SCORE_ZERO = Score(0)
  }
}

fun make_score(mg: Int, eg: Int): Score {
  return Score((eg shl 16) + mg)
}

/// Extracting the signed lower and upper 16 bits is not so trivial because
/// according to the standard a simple cast to short is implementation defined
/// and so is a right shift of a signed integer.
// TODO
//inline Value eg_value(Score s) {
//  union { uint16_t u; int16_t s; } eg = { uint16_t(unsigned(s + 0x8000) shr 16) };
//  return Value(eg.s);
//}

// TODO
//inline Value mg_value(Score s) {
//  union { uint16_t u; int16_t s; } mg = { uint16_t(unsigned(s)) };
//  return Value(mg.s);
//}

/// Division of a Score must be handled separately for each term
//inline Score operator/(Score s, int i) {
//  return make_score(mg_value(s) / i, eg_value(s) / i);
//}

/// Multiplication of a Score by an integer. We check for overflow in debug mode.
// TODO
//inline Score operator*(Score s, int i) {
//
//  Score result = Score(int(s) * i);
//
//  assert(eg_value(result) == (i * eg_value(s)));
//  assert(mg_value(result) == (i * mg_value(s)));
//  assert((i == 0) || (result / i) == s);
//
//  return result;
//}

fun Color.inv(): Color {
  return Color(v xor Color.BLACK.v)
}

// TODO
//fun Square.inv(): Square {
//  return Square(v xor SQ_A8) // Vertical flip SQ_A1 -> SQ_A8
//}

//constexpr File operator~(File f) {
//  return File(f ^ FILE_H); // Horizontal flip FILE_A -> FILE_H
//}

fun Piece.inv(): Piece {
  return Piece(v xor 8) // Swap color of piece B_KNIGHT -> W_KNIGHT
}

// TODO
//constexpr CastlingRight operator|(Color c, CastlingSide s) {
//  return CastlingRight(WHITE_OO shl ((s == QUEEN_SIDE) + 2 * c));
//}

fun Value.mate_in(ply: Int): Value {
  return Value(Value.VALUE_MATE.v - ply)
}

fun Value.mated_in(ply: Int): Value {
  return Value(-Value.VALUE_MATE.v + ply)
}

fun make_square(f: File, r: Rank): Square {
  return Square((r.v shl 3) + f.v)
}

fun make_piece(c: Color, pt: PieceType): Piece {
  return Piece((c.v shl 3) + pt.v)
}

fun type_of(pc: Piece): PieceType {
  return PieceType(pc.v and 7)
}

fun color_of(pc: Piece): Color {
  assert(pc != Piece.NO_PIECE);
  return Color(pc.v shr 3)
}

fun is_ok(s: Square): Boolean {
  return s.v >= Square.SQ_A1 && s.v <= Square.SQ_H8
}

fun file_of(s: Square): File {
  return File(s.v and 7)
}

fun rank_of(s: Square): Rank {
  return Rank(s.v shr 3)
}

fun relative_square(c: Color, s: Square): Square {
  return Square(s.v xor (c.v * 56))
}

fun relative_rank(c: Color, r: Rank): Rank {
  return Rank(r.v xor (c.v * 7))
}

fun relative_rank(c: Color, s: Square): Rank {
  return relative_rank(c, rank_of(s))
}

fun pawn_push(c: Color): Direction {
  return if (c == Color.WHITE) Direction.NORTH else Direction.SOUTH
}

fun from_sq(m: Move): Square {
  return Square((m.v shr 6) and 0x3F)
}

fun to_sq(m: Move): Square {
  return Square(m.v and 0x3F)
}

fun from_to(m: Move): Int {
 return m.v and 0xFFF
}

fun type_of(m: Move): MoveType {
  return MoveType(m.v and (3 shl 14))
}

fun promotion_type(m: Move): PieceType {
  return PieceType(((m.v shr 12) and 3) + PieceType.KNIGHT.v)
}

fun make_move(from: Square, to: Square): Move {
  return Move((from.v shl 6) + to.v)
}

// TODO
//template<MoveType T>
//constexpr Move make(Square from, Square to, PieceType pt = KNIGHT) {
//  return Move(T + ((pt - KNIGHT) shl 12) + (from shl 6) + to);
//}

fun is_ok(m: Move): Boolean {
  return from_sq(m) != to_sq(m) // Catch MOVE_NULL and MOVE_NONE
}
