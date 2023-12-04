#include <vector>
#include <string>
#include <climits>
#include <bitset>
#include "constants.h"
using namespace std;

#ifndef BOARD_H
#define BOARD_H

// returns the color of the piece, given its number, 0 for white, 1 for black
// white pieces are 1-6, black 8-14 (0 = empty square)
#define pCol(i) ((i)/7)
// value from 0-5 indicating color neutral piece type
#define pType(i) ((i)%7)
// flips square e.g a1 -> h8
#define mirror(i) ((i)^56)
// return the row of a square
#define row(i) ((i)/8)
// return the column of a square
#define col(i) ((i)%8)
// inverts player
#define opp(i) (1-(i))
// toggles ith bit of a
#define toggle(a,i) (a ^= 1ULL << (i))
// toggles ith and jth bits
// used when ith bit = 1, jth bit = 0
#define moveBit(a,i,j) (a ^= 1ULL << (i) | 1ULL << (j))
// sets bits in a which are also in b to 0
#define remove(a,b) (a &= ~(b))
// gets the zobrist index given a piece, and sq
#define zobristPieceIndex(piece,sq) ((pType(piece)-1 + pCol(piece)*6)*64+(sq))

class moveList {
public:
    int32_t len = 0;
    uint16_t container[256];

    moveList(){}
    void push(uint16_t m), swap(int32_t i, int32_t j);
    uint16_t operator[](int32_t i);
};

string bin(uint64_t n);

// gets index given magic number, blocker bitboard, and shift
// MAGIC BITBOARDS: https://www.chessprogramming.org/Magic_Bitboards
#define index(bb, magic, shift) ((int32_t) (bb*magic >> shift))

// moves are stored in uint16. 6 bits for start/end squares each. 2 bits for special flag, 2 bits for promotion piece
// these extract information from a uint16_t move
#define square1(m) ((int32_t) m&63)
#define square2(m) ((int32_t) ((m & 4032) >> 6))
#define promotion(m) ((int32_t) ((m & 16128) >> 12)+2)
#define special(m) ((int32_t) (m & 64512) >> 14)

// function to generate a uint16 given move info
inline uint16_t getShort(uint32_t square1, uint32_t square2, uint32_t promote=0, uint32_t special=0){
    return (uint16_t) (square1 | square2 << 6 | promote << 12 | special << 14); 
}

// prints move
string showMove(uint16_t m = 0, int32_t result = 0, bool useResult = false);

int32_t poplsb(uint64_t &n, bool remove = true);

uint64_t intToBlocker(int32_t n, uint64_t mask);

int32_t count1s(uint64_t n);

inline bool singular(uint64_t n){
    poplsb(n);
    return n == 0;
}

uint64_t rookMask(int32_t sq, int32_t edge = 0, uint64_t bb = 0);

uint64_t bishopMask(int32_t sq, int32_t edge = 0, uint64_t bb = 0);

uint64_t knightMask(int32_t i);

uint64_t kingMask(int32_t i);

uint64_t pawnMask(int32_t sq, bool player);

class board {
public:
    int32_t sqs[64] = {};
    uint64_t bitbs[2][7] = {};
    // random numbers are assigned to each feature of a position. for example white can castle kingside, it is blacks move, there is a black knight on a1 are all features
    // the zobrist key is obtained by xoring all features of a position
    uint64_t zobrist;
    // castle[i][j] is the turn on which ith player lost the right to castle on the jth side. castle[i][j] = MAX32 if castling is available, and -1 if castling was never available.
    // this is necessary for unmaking moves
    int32_t castle[2][2] = {MAX32,MAX32,MAX32,MAX32};
    uint16_t gameHist[5898] = {};
    int32_t captured[5898] = {};
    int32_t gameLen = 0;

    int32_t player = 0;
    int32_t fiftyCount = 0;
    int32_t phase = 0;
    // repetion.back() is all positions which need to be searched when checking for 3-move repetition
    // not all positions need to be searched as after a capture/pawn move it is impossible that positions after will repeat positions before
    // when this occurs we do repetition.push_back(empty vector)
    // positions are stored by zobrist key
    vector<vector<uint64_t>> repetition;
    uint64_t pins[2];
    bool legal,quiesce;
    moveList moves;
    
    moveList genMoves(bool legal_, bool quiesce_);
    int32_t eval(), val(int32_t sq, bool simple = false);
    board(string fen = START_FEN);
    string toString(), printBB(), fen();
    bool makeMove(uint16_t m), attacked(int32_t sq = -1);
    int32_t lva(int32_t sq, uint64_t traded, int32_t p = -1);
    void unmakeMove();
    bitset<781> bits;

private:
    uint64_t pawnAttacks(int32_t sq, bool moveGen, bool removeSame, int32_t p = -1), rookAttacks(int32_t sq, bool removeSame, int32_t p = -1, uint64_t omitBB = 0ULL), knightAttacks(int32_t sq, bool removeSame, int32_t p = -1), bishopAttacks(int32_t sq, bool removeSame, int32_t p = -1, uint64_t omitBB = 0ULL), queenAttacks(int32_t sq, bool removeSame, int32_t p = -1, uint64_t omitBB = 0ULL), kingAttacks(int32_t sq, bool removeSame, int32_t p = -1);
    bool canCastle(int32_t i, int32_t j), occursTwice(uint64_t a);
    void pushMove(uint16_t m), genPawnMoves(), genRookMoves(), genKnightMoves(), genBishopMoves(), genQueenMoves(), genKingMoves(), genPinMasks(int32_t p = -1, uint64_t traded = 0);
};

uint16_t shortFromAlgebraic(string a, board* b);

inline string algebraicSquare(int32_t sq){
    string s = {char(col(sq)+int32_t('a')), char(row(sq)+int32_t('1'))};
    return s;
}

string algebraicFromShort(uint16_t m);

#endif