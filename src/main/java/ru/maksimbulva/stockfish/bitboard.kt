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

object Bitbases {
    fun init() {
        TODO()
    }

    fun probe(wksq: Square, wpsq: Square, bksq: Square, us: Color): Boolean {
        TODO()
    }
}

object Bitboards {

    fun init() {
        TODO()
    }

    fun pretty(b: Bitboard): String {
        TODO()
    }
}

inline class Bitboard(val v: Long) {

    fun isSet(square: Square): Boolean {
        return (v and (1L shl square.v)) != 0L
    }

    companion object {
        const val AllSquares: Long = 0L.inv()
        val DarkSquares = Bitboard(0xAA55AA55AA55AA55UL.toLong())

        const val FileABB = 0x0101010101010101L
        const val FileBBB = FileABB shl 1
        const val FileCBB = FileABB shl 2
        const val FileDBB = FileABB shl 3
        const val FileEBB = FileABB shl 4
        const val FileFBB = FileABB shl 5
        const val FileGBB = FileABB shl 6
        const val FileHBB = FileABB shl 7

        const val Rank1BB: Long = 0xFF
        const val Rank2BB = Rank1BB shl (8 * 1)
        const val Rank3BB = Rank1BB shl (8 * 2)
        const val Rank4BB = Rank1BB shl (8 * 3)
        const val Rank5BB = Rank1BB shl (8 * 4)
        const val Rank6BB = Rank1BB shl (8 * 5)
        const val Rank7BB = Rank1BB shl (8 * 6)
        const val Rank8BB = Rank1BB shl (8 * 7)

        const val QueenSide   = FileABB or FileBBB or FileCBB or FileDBB
        const val CenterFiles = FileCBB or FileDBB or FileEBB or FileFBB
        const val KingSide    = FileEBB or FileFBB or FileGBB or FileHBB
        const val Center      = (FileDBB or FileEBB) and (Rank4BB or Rank5BB)
    }
}

// TODO
//extern uint8_t PopCnt16[1 shl 16]
//extern uint8_t SquareDistance[SQUARE_NB][SQUARE_NB]

// TODO
//extern Bitboard LineBB[SQUARE_NB][SQUARE_NB]
//extern Bitboard PseudoAttacks[PIECE_TYPE_NB][SQUARE_NB]
//extern Bitboard PawnAttacks[COLOR_NB][SQUARE_NB]
//extern Bitboard KingFlank[FILE_NB]
//extern Bitboard SquareBB[SQUARE_NB]


/// Magic holds all magic bitboards relevant data for a single square
class Magic(
        private val mask: Bitboard,
        private val magic: Bitboard,
        private val attacks: Bitboard,
        private val shift: Int
) {
  // Compute the attack's index using the 'magic bitboards' approach
  // TODO - change ret type to unsigned int if needed
  fun index(occupied: Bitboard): Int {

    return (((occupied.v and mask.v) * magic.v) ushr shift).toInt()

      // TODO - consider this code for 32-bit instead
//    unsigned lo = unsigned(occupied) & unsigned(mask)
//    unsigned hi = unsigned(occupied >> 32) & unsigned(mask >> 32)
//    return (lo * unsigned(magic) ^ hi * unsigned(magic >> 32)) >> shift
  }
}

// TODO
//extern Magic RookMagics[SQUARE_NB]
//extern Magic BishopMagics[SQUARE_NB]

fun square_bb(s: Square): Bitboard {
    assert(s.v >= Square.SQ_A1 && s.v <= Square.SQ_H8)
    TODO()
  // return SquareBB[s]
}

fun more_than_one(b: Bitboard): Boolean {
  return (b.v and (b.v - 1)) != 0L
}

fun opposite_colors(s1: Square, s2: Square): Boolean {
  return Bitboard.DarkSquares.isSet(s1) != Bitboard.DarkSquares.isSet(s2)
}


/// rank_bb() and file_bb() return a bitboard representing all the squares on
/// the given file or rank.

fun rank_bb(r: Rank): Bitboard {
  return Bitboard(Bitboard.Rank1BB shl (8 * r.v))
}

fun rank_bb(s: Square): Bitboard {
  return rank_bb(rank_of(s))
}

fun file_bb(f: File): Bitboard {
  return Bitboard(Bitboard.FileABB shl f.v)
}

fun file_bb(s: Square): Bitboard {
  return file_bb(file_of(s))
}


/// shift() moves a bitboard one step along direction D

fun shift(bitboard: Bitboard, direction: Direction): Bitboard {
    val v = bitboard.v
    return Bitboard(when (direction) {
        Direction.NORTH -> v shl 8
        Direction.SOUTH -> v ushr 8
        Direction.EAST -> (v and Bitboard.FileHBB.inv()) shl 1
        Direction.WEST -> (v and Bitboard.FileABB.inv()) ushr 1
        Direction.NORTH_EAST -> (v and Bitboard.FileHBB.inv()) shl 9
        Direction.NORTH_WEST -> (v and Bitboard.FileABB.inv()) shl 7
        Direction.SOUTH_EAST -> (v and Bitboard.FileHBB.inv()) ushr 7
        Direction.SOUTH_WEST -> (v and Bitboard.FileABB.inv()) ushr 9
        else -> 0L
    })
}

/// pawn_attacks_bb() returns the squares attacked by pawns of the given color
/// from the squares in the given bitboard.

// TODO
//template<Color C>
//constexpr Bitboard pawn_attacks_bb(Bitboard b) {
//  return C == WHITE ? shift<NORTH_WEST>(b) or shift<NORTH_EAST>(b)
//                    : shift<SOUTH_WEST>(b) or shift<SOUTH_EAST>(b)
//}


/// pawn_double_attacks_bb() returns the squares doubly attacked by pawns of the
/// given color from the squares in the given bitboard.

// TODO
//template<Color C>
//constexpr Bitboard pawn_double_attacks_bb(Bitboard b) {
//  return C == WHITE ? shift<NORTH_WEST>(b) & shift<NORTH_EAST>(b)
//                    : shift<SOUTH_WEST>(b) & shift<SOUTH_EAST>(b)
//}


/// adjacent_files_bb() returns a bitboard representing all the squares on the
/// adjacent files of the given one.

// TODO - maybe replace this with array of precomputed values
fun adjacent_files_bb(f: File): Bitboard {
    val bitboard = file_bb(f)
    return Bitboard(shift(bitboard, Direction.EAST).v or shift(bitboard, Direction.WEST).v)
}


/// between_bb() returns squares that are linearly between the given squares
/// If the given squares are not on a same file/rank/diagonal, return 0.

// TODO
//inline Bitboard between_bb(Square s1, Square s2) {
//  return LineBB[s1][s2] & ( (AllSquares shl (s1 +  (s1 < s2)))
//                           ^(AllSquares shl (s2 + !(s1 < s2))))
//}


/// forward_ranks_bb() returns a bitboard representing the squares on the ranks
/// in front of the given one, from the point of view of the given color. For instance,
/// forward_ranks_bb(BLACK, SQ_D3) will return the 16 squares on ranks 1 and 2.

fun forward_ranks_bb(c: Color, s: Square): Bitboard {
    return Bitboard(if (c == Color.WHITE) {
        Bitboard.Rank1BB.inv() shl 8 * (rank_of(s).v - Rank.RANK_1)
    } else {
        Bitboard.Rank8BB.inv() ushr 8 * (Rank.RANK_8 - rank_of(s).v)
    })
}


/// forward_file_bb() returns a bitboard representing all the squares along the
/// line in front of the given one, from the point of view of the given color.

fun forward_file_bb(c: Color, s: Square): Bitboard {
  return Bitboard(forward_ranks_bb(c, s).v and file_bb(s).v)
}


/// pawn_attack_span() returns a bitboard representing all the squares that can
/// be attacked by a pawn of the given color when it moves along its file,
/// starting from the given square.

fun pawn_attack_span(c: Color, s: Square): Bitboard {
  return Bitboard(forward_ranks_bb(c, s).v and adjacent_files_bb(file_of(s)).v)
}


/// passed_pawn_span() returns a bitboard which can be used to test if a pawn of
/// the given color and on the given square is a passed pawn.

fun passed_pawn_span(c: Color, s: Square): Bitboard {
  return Bitboard(
          forward_ranks_bb(c, s).v and (adjacent_files_bb(file_of(s)).v or file_bb(s).v)
  )
}


/// aligned() returns true if the squares s1, s2 and s3 are aligned either on a
/// straight or on a diagonal line.

fun aligned(s1: Square, s2: Square, s3: Square): Boolean {
    TODO()
//  return LineBB[s1][s2] & s3
}


/// distance() functions return the distance between x and y, defined as the
/// number of steps for a king in x to reach y.

// TODO
//template<typename T1 = Square> inline int distance(Square x, Square y)
//template<> inline int distance<File>(Square x, Square y) { return std::abs(file_of(x) - file_of(y)) }
//template<> inline int distance<Rank>(Square x, Square y) { return std::abs(rank_of(x) - rank_of(y)) }
//template<> inline int distance<Square>(Square x, Square y) { return SquareDistance[x][y] }
//
//template<class T> constexpr const T& clamp(const T& v, const T& lo, const T&  hi) {
//  return v < lo ? lo : v > hi ? hi : v
//}

/// attacks_bb() returns a bitboard representing all the squares attacked by a
/// piece of type Pt (bishop or rook) placed on 's'.

// TODO
//template<PieceType Pt>
//inline Bitboard attacks_bb(Square s, Bitboard occupied) {
//
//  const Magic& m = Pt == ROOK ? RookMagics[s] : BishopMagics[s]
//  return m.attacks[m.index(occupied)]
//}

// TODO
//fun attacks_bb(pt: PieceType, s: Square, occupied: Bitboard): Bitboard {
//
//    assert(pt != PieceType.PAWN)
//
//    return when (pt) {
//        PieceType.BISHOP -> attacks_bb<BISHOP>(s, occupied)
//        PieceType.ROOK -> attacks_bb<ROOK>(s, occupied)
//        PieceType.QUEEN -> attacks_bb<BISHOP>(s, occupied)
//            or attacks_bb<ROOK>(s, occupied)
//        else -> PseudoAttacks[pt][s]
//    }
//}


/// popcount() counts the number of non-zero bits in a bitboard

//  TODO
//inline int popcount(Bitboard b) {
//
//#ifndef USE_POPCNT
//
//  union { Bitboard bb; uint16_t u[4]; } v = { b };
//  return PopCnt16[v.u[0]] + PopCnt16[v.u[1]] + PopCnt16[v.u[2]] + PopCnt16[v.u[3]];
//
//#elif defined(_MSC_VER) || defined(__INTEL_COMPILER)
//
//  return (int)_mm_popcnt_u64(b);
//
//#else // Assumed gcc or compatible compiler
//
//  return __builtin_popcountll(b);
//
//#endif
//}


/// lsb() and msb() return the least/most significant bit in a non-zero bitboard

// TODO
//#if defined(__GNUC__)  // GCC, Clang, ICC
//
//inline Square lsb(Bitboard b) {
//  assert(b);
//  return Square(__builtin_ctzll(b));
//}
//
//inline Square msb(Bitboard b) {
//  assert(b);
//  return Square(63 ^ __builtin_clzll(b));
//}
//
//#elif defined(_MSC_VER)  // MSVC
//
//#ifdef _WIN64  // MSVC, WIN64
//
//inline Square lsb(Bitboard b) {
//  assert(b);
//  unsigned long idx;
//  _BitScanForward64(&idx, b);
//  return (Square) idx;
//}
//
//inline Square msb(Bitboard b) {
//  assert(b);
//  unsigned long idx;
//  _BitScanReverse64(&idx, b);
//  return (Square) idx;
//}
//
//#else  // MSVC, WIN32
//
//inline Square lsb(Bitboard b) {
//  assert(b);
//  unsigned long idx;
//
//  if (b & 0xffffffff) {
//      _BitScanForward(&idx, int32_t(b));
//      return Square(idx);
//  } else {
//      _BitScanForward(&idx, int32_t(b >> 32));
//      return Square(idx + 32);
//  }
//}
//
//inline Square msb(Bitboard b) {
//  assert(b);
//  unsigned long idx;
//
//  if (b >> 32) {
//      _BitScanReverse(&idx, int32_t(b >> 32));
//      return Square(idx + 32);
//  } else {
//      _BitScanReverse(&idx, int32_t(b));
//      return Square(idx);
//  }
//}
//
//#endif
//
//#else  // Compiler is neither GCC nor MSVC compatible
//
//#error "Compiler not supported."
//
//#endif


/// pop_lsb() finds and clears the least significant bit in a non-zero bitboard

// TODO
//inline Square pop_lsb(Bitboard* b) {
//  const Square s = lsb(*b);
//  *b &= *b - 1;
//  return s;
//}


/// frontmost_sq() and backmost_sq() return the square corresponding to the
/// most/least advanced bit relative to the given color.

// TODO
//inline Square frontmost_sq(Color c, Bitboard b) { return c == WHITE ? msb(b) : lsb(b); }
//inline Square  backmost_sq(Color c, Bitboard b) { return c == WHITE ? lsb(b) : msb(b); }
