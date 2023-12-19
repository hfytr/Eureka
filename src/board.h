#ifndef BOARD_H
#define BOARD_H
#include <vector>
#include <string>
#include <climits>
#include <bitset>
#include "constants.h"

#define col(sq) ((sq) & 7)
#define row(sq) ((sq) >> 3)
#define mirror(sq) ((sq)^56)
#define abs(i) ((i) > 0 ? (i) : -(i))

enum PieceType {
    EMPTY = 0,
    PAWN = 2,
    ROOK = 3,
    KNIGHT = 4,
    BISHOP = 5,
    QUEEN = 6,
    KING = 7
};
enum Color {
    WHITE = 0,
    BLACK = 1
};

#define opp(c) ((c) == WHITE ? BLACK : WHITE)

class Piece {
public:
    Piece(char c = 0);
    Piece(const Piece& p) = default;
    Piece(PieceType p, Color c) { color = c; type = p; }
    Piece& operator=(const Piece& p) = default;
    [[nodiscard]] Color getColor() const { return color; }
    [[nodiscard]] PieceType getType() const { return type; }
    [[nodiscard]] char letter() const { return type == EMPTY ? '.' : letterFromNum[color*6 + type-2]; }

private:
    static constexpr char letterFromNum[12] = {'P', 'R', 'N', 'B', 'Q', 'K', 'p', 'r', 'n', 'b', 'q', 'k'};
    constexpr static PieceType asciiToPieceType[26] = {EMPTY, BISHOP, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, KING, EMPTY, EMPTY, KNIGHT, EMPTY, PAWN, QUEEN, ROOK, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY};

    PieceType type = EMPTY;
    Color color = WHITE;
};

enum MoveType {
    NONE = 0,
    EP = 1,
    PROMOTE = 2,
    CASTLE = 3
};

class Move {
public:
    // 16 bit move, bits 0-5 are square 1, bits 6-11 square 2, bits 12-13 promotion piece, bits 14-15 special flag (normal, promotion, castling, or en pessant)
    Move (uint8_t square1=0, uint8_t square2=0, PieceType promote=ROOK, MoveType special=NONE){
        container = square1 | square2 << 6 | (static_cast<uint8_t>(promote)-3) << 12 | special << 14;
    }

    [[nodiscard]] MoveType special() const { return static_cast<MoveType>((container & 0xc000) >> 14); }
    [[nodiscard]] PieceType promotion() const { return static_cast<PieceType>(((container & 0x3000) >> 12) + 3); }
    [[nodiscard]] uint8_t square2() const { return (container & 0xfc0) >> 6; }
    [[nodiscard]] uint8_t square1() const { return container & 0x3f; }
    [[nodiscard]] bool isNull() const { return container == 0; }
    [[nodiscard]] bool isDoublePush(Piece p) const { return abs(square2() - square1()) == 16 && p.getType() == PAWN; }
    [[nodiscard]] uint16_t raw() const { return container; }
    [[nodiscard]] bool operator==(Move& m) const { return m.container == container; }

    std::string print();

protected:
    uint16_t container;
};

class moveList {
public:
    moveList() = default;
    void push(Move m);
    void swap(int32_t i, int32_t j);
    Move operator[](int32_t i);
    virtual uint8_t len(){ return length; }

protected:
    uint8_t length = 0;
    Move container[255];
};

class Board;
class Zobrist {
public:
    Zobrist(uint64_t ll = 0) { container = ll; }
    Zobrist(Board* b);
    uint64_t getKey() const { return container; }
    bool operator==(Zobrist& z) const { return z.container == container; }
    bool operator!=(Zobrist& z) const { return z.container != container; }
    void toggleSquare(Piece p, uint8_t sq) {
        container ^= zrn[pieceIndex(p, sq)];
        bits.flip(pieceIndex(p, sq));
    }
    void toggleCastle(Color stm, bool kingside) {
        container ^= zrn[ZCASTLE + stm*2 + kingside];
        bits.flip(ZCASTLE + stm*2 + kingside);
    }
    void toggleEP(uint8_t file) {
        container ^= zrn[ZEP + file];
        bits.flip(ZEP + file);
    }
    void toggleSTM() {
        container ^= zrn[ZPLAYER];
        bits.flip(ZPLAYER);
    }
    std::bitset<781> getBits() { return bits; }

private:
    static uint16_t pieceIndex(const Piece& p, uint8_t sq) { return (p.getType()-2 + p.getColor()*6) * 64 + sq; }
    uint64_t container = 0ULL;
    std::bitset<781> bits;
};

class Bitboard {
public:
    Bitboard(uint64_t ll = 0){ container = ll; }
    Bitboard(Piece p, uint8_t sq, Bitboard blockers);

    static void initaliseAttackLookups();

    [[nodiscard]] std::string print() const;
    [[nodiscard]] uint8_t count1s() const;
    uint8_t poplsb(bool remove = true);
    [[nodiscard]] uint64_t raw() const { return container; }

    void toggle(uint8_t i) { container ^= 1ULL << i; }
    void moveBit(uint8_t i, uint8_t j) { container ^= 1ULL << i | 1ULL << j; }
    void remove(Bitboard bb) { container &= ~bb.container; }

    Bitboard& operator=(const Bitboard& bb) = default;
    Bitboard& operator&=(const Bitboard& bb);
    Bitboard& operator|=(const Bitboard& bb);
    Bitboard& operator>>=(uint8_t n);
    Bitboard operator~() const { return ~container; }
    Bitboard operator|(const Bitboard& bb) const { return container | bb.container; }
    Bitboard operator&(const Bitboard& bb) const { return container & bb.container; }
    Bitboard operator<<(const Bitboard& bb) const { return container << bb.container; }
    Bitboard operator>>(const Bitboard& bb) const { return container >> bb.container; }

private:
    uint64_t container = 0;
    static std::vector<Bitboard> attackLookup[6];
    static Bitboard slidingAttackLookup[2][64][4096];

    static Bitboard genRookLookup(uint8_t sq, Bitboard blockers = 0ULL, bool edge = true);
    static Bitboard genBishopLookup(uint8_t sq, Bitboard blockers = 0ULL, bool edge = true);
    static Bitboard genPawnLookup(uint8_t sq, Color player = WHITE);
    static Bitboard genKnightLookup(uint8_t sq);
    static Bitboard genKingLookup(uint8_t sq);

    static uint16_t magicIndex(uint8_t sq, Bitboard blockers, PieceType p);
    static Bitboard intToBlocker(uint16_t i, Bitboard attacks);
};

class Board {
public:
    Board(std::string fen = START_FEN);
    bool makeMove(Move m);
    void unmakeMove();
    moveList genMoves(bool legal_, bool quiece_);
    bool canCastle(int32_t i, int32_t j) { return castle[i][j] == CANCASTLE; }

    [[nodiscard]] Bitboard getbb(uint8_t i) const { return bitbs[i]; }
    [[nodiscard]] Piece getsq(uint8_t i) const { return sqs[i]; }
    [[nodiscard]] Color getstm() const { return stm; }
    [[nodiscard]] Move getHist(uint8_t i) const { return gameHist[i]; }
    [[nodiscard]] Zobrist getZobrist() const { return zobrist; }
    [[nodiscard]] uint16_t getLen() const { return gameLen; }

    int32_t eval();
    int32_t val(uint8_t sq, bool simple = false);

    std::string print();
    std::string printBitboards(uint8_t i = 8);
    std::string fen();

    bool attacked(uint8_t sq = 64);
    bool isOver();
    uint8_t lva(uint8_t sq, Bitboard traded, Color p);

private:
    Bitboard bitbs[8];
    Piece sqs[64] = {};
    uint16_t castle[2][2] = {CANCASTLE,CANCASTLE,CANCASTLE,CANCASTLE};
    Color stm = WHITE; // side to move

    bool occursThrice(Zobrist z);
    void pushMove(Move m);
    std::pair<Bitboard, uint8_t> genAttacks(Piece p, uint8_t sq, bool lva);
    void genCastle(uint8_t piece);

    Move gameHist[5898] = {};
    Piece captured[5898] = {};
    uint8_t fiftyCount[5898] = {};
    uint16_t gameLen = 0;
    uint8_t phase = 0;

    Zobrist zobrist;
    std::vector<std::vector<Zobrist>> repetition;

    Bitboard pins[2];
    bool legal = false, quiesce = false;
    moveList moves;
};

class Algebraic {
public:
    static Move constructMove(std::string s, Board* b);
    static std::string square(uint8_t sq);
    static std::string move(Move m);
};
#endif