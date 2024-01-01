#include <vector>
#include <string>
#include <cmath>
#include <iostream>
#include <algorithm>
#include "board.h"
#include "constants.h"

Piece::Piece(char c) {
    if (c < 'Z') {
        type = asciiToPieceType[c - 'A'];
        color = WHITE;
    } else {
        type = asciiToPieceType[c - 'a'];
        color = BLACK;
    }
}

std::string Move::print() {
    return "algebraic: " + Algebraic::move(*this) + " raw: " + std::to_string(container) + " sq1: " + std::to_string(square1()) + " sq2: " + std::to_string(square2()) + " prom: " + std::to_string(promotion()) + " special: " + std::to_string(special()) + "\n";
}

// uses DeBruijn sequences (https://www.chessprogramming.org/BitScan)
uint8_t Bitboard::poplsb(bool remove) {
    if (container == 0)
        return 64;
    int32_t i = DeBruijnLookup[((container & -container) * DeBruijn) >> 58];
    if (remove)
        toggle(i);
    return i;
}

uint8_t Bitboard::count1s() const {
    Bitboard temp = container;
    int32_t i = 0;
    while (temp.raw() != 0){
        temp.poplsb();
        i++;
    }
    return i;
}

std::string Bitboard::print() const {
    std::string rows[8];
    Bitboard temp = container;
    for (int32_t i = 0; i < 8; i++) {
        for (int32_t j = 0; j < 8; j++) {
            rows[i] += std::to_string(temp.raw() & 1ULL);
            temp >>= 1;
        }
        rows[i] += '\n';
    }
    std::string s;
    for (int32_t i = 7; i >= 0; i--)
        s += rows[i];
    return s + '\n';
}

Bitboard &Bitboard::operator&=(const Bitboard &bb) {
    container &= bb.raw();
    return *this;
}

Bitboard &Bitboard::operator>>=(uint8_t n) {
    container >>= n;
    return *this;
}

Bitboard &Bitboard::operator|=(const Bitboard &bb) {
    container |= bb.raw();
    return *this;
}

// returns attack bitboard given square, whether to include the edge of the board (1 or 0) and blockers pieces
// blockers are treated as ENEMY pieces -> the edge can always be attacked and is sometimes unnecessary
Bitboard Bitboard::genRookLookup(uint8_t sq, Bitboard bb, bool edge) {
    Bitboard tr = 0;
    for (int32_t i = col(sq); i < 7 + edge; i++) {
        if (i != col(sq))
            tr.toggle(sq - col(sq) + i);
        if (bb.raw() & 1ULL << (sq - col(sq) + i))
            break;
    }
    for (int32_t i = col(sq); i > 0 - edge; i--) {
        if (i != col(sq))
            tr.toggle(sq - col(sq) + i);
        if (bb.raw() & 1ULL << (sq - col(sq) + i))
            break;
    }
    for (int32_t i = row(sq); i < 7 + edge; i++) {
        if (i != row(sq))
            tr.toggle(col(sq) + i * 8);
        if (bb.raw() & 1ULL << (col(sq) + i * 8))
            break;
    }
    for (int32_t i = row(sq); i > 0 - edge; i--) {
        if (i != row(sq))
            tr.toggle(col(sq) + i * 8);
        if (bb.raw() & 1ULL << (col(sq) + i * 8))
            break;
    }
    return tr;
}

Bitboard Bitboard::genBishopLookup(uint8_t sq, Bitboard bb, bool edge) {
    Bitboard tr = 0;
    for (int32_t i = 1; i < std::min(7 + edge - row(sq), 7 + edge - col(sq)); i++) {
        tr.toggle((row(sq) + i) * 8 + col(sq) + i);
        if (bb.raw() & 1ULL << ((row(sq) + i) * 8 + col(sq) + i))
            break;
    }
    for (int32_t i = 1; i < std::min(7 + edge - row(sq), col(sq) + edge); i++) {
        tr.toggle((row(sq) + i) * 8 + col(sq) - i);
        if (bb.raw() & 1ULL << ((row(sq) + i) * 8 + col(sq) - i))
            break;
    }
    for (int32_t i = 1; i < std::min(row(sq) + edge, 7 + edge - col(sq)); i++) {
        tr.toggle((row(sq) - i) * 8 + col(sq) + i);
        if (bb.raw() & 1ULL << ((row(sq) - i) * 8 + col(sq) + i))
            break;
    }
    for (int32_t i = 1; i < std::min(row(sq) + edge, col(sq) + edge); i++) {
        tr.toggle((row(sq) - i) * 8 + col(sq) - i);
        if (bb.raw() & 1ULL << ((row(sq) - i) * 8 + col(sq) - i))
            break;
    }
    return tr;
}

Bitboard Bitboard::genKnightLookup(uint8_t sq) {
    int32_t dir[8][2] = {{2,  1},
                         {2,  -1},
                         {-2, 1},
                         {-2, -1},
                         {1,  2},
                         {1,  -2},
                         {-1, 2},
                         {-1, -2}};
    Bitboard tr = 0;
    for (int32_t j = 0; j < 8; j++)
        if (dir[j][1] + row(sq) >= 0 &&
            dir[j][1] + row(sq) < 8 &&
            dir[j][0] + col(sq) >= 0 &&
            dir[j][0] + col(sq) < 8)
            tr.toggle((dir[j][1] + row(sq)) * 8 + dir[j][0] + col(sq));
    return tr;
}

Bitboard Bitboard::genKingLookup(uint8_t sq) {
    int32_t dir[8][2] = {{0,  1},
                         {1,  0},
                         {-1, 0},
                         {0,  -1},
                         {1,  1},
                         {1,  -1},
                         {-1, 1},
                         {-1, -1}};
    Bitboard tr = 0;
    for (int32_t j = 0; j < 8; j++)
        if (row(sq) + dir[j][0] >= 0 &&
            row(sq) + dir[j][0] < 8 &&
            col(sq) + dir[j][1] >= 0 &&
            col(sq) + dir[j][1] < 8)
            tr.toggle((row(sq) + dir[j][0]) * 8 + (col(sq) + dir[j][1]));
    return tr;
}

// only includes captures
Bitboard Bitboard::genPawnLookup(uint8_t sq, Color stm) {
    int32_t shift = stm ? -1 : 1;
    Bitboard tr = 0;
    if (abs(row(sq) - row(sq + shift * 7)) == 1)
        tr.toggle(sq + shift * 7);
    if (abs(row(sq) - row(sq + shift * 9)) == 1)
        tr.toggle(sq + shift * 9);
    return tr;
}

// generates the ith blockers bitboard given an attack mask.
// used for magic number generation / initialisation ofmagic tables
Bitboard Bitboard::intToBlocker(uint16_t i, Bitboard attacks) {
    Bitboard tr = 0;
    uint8_t cur;
    uint16_t j = i;
    while (attacks.container) {
        cur = attacks.poplsb();
        if (i % 2)
            tr.toggle(cur);
        i >>= 1;
    }
    return tr;
}

uint16_t Bitboard::magicIndex(uint8_t sq, Bitboard blockers, PieceType p) {
    switch (p) {
        case ROOK:
            return (rookMagics[sq] * blockers.container) >> rookShift[sq];
        case BISHOP:
            return (bishopMagics[sq] * blockers.container) >> bishopShift[sq];
        default:
            return 0;
    }
}

std::vector<Bitboard> Bitboard::attackLookup[6];
Bitboard Bitboard::slidingAttackLookup[2][64][4096];
void Bitboard::initaliseAttackLookups() {
    for (uint8_t i = PAWN; i <= KING; i++)
        attackLookup[i-2].resize(i == PAWN ? 128 : 64);
    for (uint8_t i = 0; i < 64; i++) {
        attackLookup[PAWN - 2][i] = genPawnLookup(i, WHITE);
        attackLookup[PAWN - 2][i + 64] = genPawnLookup(i, BLACK);
        attackLookup[ROOK - 2][i] = genRookLookup(i, 0, false);
        attackLookup[KNIGHT - 2][i] = genKnightLookup(i);
        attackLookup[BISHOP - 2][i] = genBishopLookup(i, 0, false);
        attackLookup[QUEEN - 2][i] = attackLookup[ROOK - 2][i] | attackLookup[BISHOP - 2][i];
        attackLookup[KING - 2][i] = genKingLookup(i);
        ;
        uint8_t n = attackLookup[ROOK-2][i].count1s();
        for (uint16_t j = 0; j < 1 << n; j++) {
            Bitboard blockers = intToBlocker(j, attackLookup[ROOK - 2][i]);
            slidingAttackLookup[0][i][magicIndex(i, blockers, ROOK)] = genRookLookup(i, blockers);
        };
        n = attackLookup[BISHOP-2][i].count1s();
        for (uint16_t j = 0; j < 1 << n; j++) {
            Bitboard blockers = intToBlocker(j, attackLookup[BISHOP - 2][i]);
            slidingAttackLookup[1][i][magicIndex(i, blockers, BISHOP)] = genBishopLookup(i, blockers);
        }
    }
}

Bitboard::Bitboard(Piece p, uint8_t sq, Bitboard blockers) {
    if (p.getType() == QUEEN) {
        uint16_t rookIndex = magicIndex(sq, blockers & attackLookup[ROOK-2][sq], ROOK);
        uint16_t bishopIndex = magicIndex(sq, blockers & attackLookup[BISHOP-2][sq], BISHOP);
        *this = slidingAttackLookup[0][sq][rookIndex] | slidingAttackLookup[1][sq][bishopIndex];
    } else if (p.getType() == ROOK || p.getType() == BISHOP) {
        uint16_t index = magicIndex(sq, blockers & attackLookup[p.getType()-2][sq], p.getType());
        *this = slidingAttackLookup[p.getType() == BISHOP][sq][index];
    } else if (p.getType() == PAWN)
        *this = attackLookup[p.getType()-2][sq + p.getColor() * 64];
    else
        *this = attackLookup[p.getType()-2][sq];
}

void moveList::push(Move m) {
    container[length] = m;
    length++;
}

Move moveList::operator[](int32_t i) {
    return container[i];
}

void moveList::swap(int32_t i, int32_t j) {
    Move temp = container[i];
    container[i] = container[j];
    container[j] = temp;
}

Zobrist::Zobrist(Board *b) {
    int32_t sq;
    Bitboard pieces = b->getbb(0) | b->getbb(1);
    while (pieces.raw()) {
        sq = pieces.poplsb();
        toggleSquare(b->getsq(sq), sq);
    }

    if (b->getstm() == BLACK)
        toggleSTM();
    for (uint8_t i = 0; i < 4; i++)
        if (b->canCastle(i / 2, i % 2))
            toggleCastle(static_cast<Color>(i / 2), i % 2);

    Move m = b->getHist(0);
    if (abs(m.square1() - m.square2()) == 16 &&
        b->getsq(m.square2()).getType() == PAWN)
        toggleEP(col(m.square2()));
}

// FEN notation: https://en.wikipedia.org/wiki/Forsyth–Edw  ards_Notation
Board::Board(std::string fen) {
    for (uint8_t i = 0; i < 64; i++) sqs[i] = Piece(EMPTY, WHITE);
    uint8_t i = 0, row = 7, col = 0;
    while (fen[i] != ' ') {
        if (fen[i] == '/' || col == 8) {
            row--;
            col = -1;
        } else if (fen[i] < '9')
            col += fen[i] - '0' - 1;
        else {
            sqs[row * 8 + col] = Piece(fen[i]);
            phase += phaseInc[sqs[row * 8 + col].getType() - 2];
            bitbs[sqs[row * 8 + col].getType()].toggle(row * 8 + col);
            bitbs[sqs[row * 8 + col].getColor()].toggle(row * 8 + col);
        }
        i++;
        col++;
    }

    i++;
    stm = fen[i] == 'w' ? WHITE : BLACK;
    i += 2;
    std::string castleChar = "KQkq";
    for (uint8_t j = 0; j < 2; j++) {
        for (uint8_t k = 0; k < 2; k++) {
            if (fen[i] != castleChar[j * 2 + k])
                castle[j][!k] = CANTCASTLE;
            if (canCastle(j, !k))
                i++;
        }
    }
    i += (fen[i] != ' ') + 1;

    // to handle ep, sets the first move as being the fen specified double pawn push
    if (fen[i] != '-') {
        int32_t sq = (int32_t(fen[i + 1]) - int32_t('1')) * 8 + int32_t(fen[i]) - int32_t('a');
        if (stm)
            gameHist[0] = Move(sq - 8, sq + 8);
        else
            gameHist[0] = Move(sq + 8, sq - 8);
    }
    i += 2;
    fiftyCount[0] = fen[i+1] == ' ' ? (fen[i] - '0') : ((fen[i] - '0') * 10 + fen[i + 1]);

    Bitboard::initaliseAttackLookups();

    // initialise piece square tables: adds the base piece value to the square value for each piece / game phase
    for (i = 0; i < 6; i++) {
        for (uint8_t j = 0; j < 64; j++) {
            mgpesto[i][j] = mgval[i] + mgpestoPre[i][mirror(j)]; // tables were provided from a mirror perspective
            egpesto[i][j] = egval[i] + egpestoPre[i][mirror(j)];
        }
    }

    zobrist = Zobrist(this);

    repetition.emplace_back(0);
    repetition.back().push_back(zobrist);
}

bool Board::isOver() {
    return genMoves(true, false).len() == 0;
}

std::string Board::fen() {
    std::string fen;
    for (int32_t i = 7; i >= 0; i--) {
        for (int32_t j = 0; j < 8; j++) {
            if (sqs[i * 8 + j].getType() == EMPTY) {
                if ((i == 7 & j == 0) || fen.back() == '/' || fen.back() > '9')
                    fen += '1';
                else
                    fen.back()++;
            } else
                fen += sqs[i * 8 + j].letter();
        }
        if (i != 0)
            fen += '/';
    }

    fen += stm ? " b " : " w ";

    std::string castleChar = "QKqk";
    for (int32_t i = 0; i < 4; i++)
        if (canCastle(i / 2, i % 2))
            fen += castleChar[i];
    if (fen.back() == ' ')
        fen += '-';

    Move m = gameHist[gameLen];
    if (sqs[m.square2()].getType() == PAWN &&
        abs(m.square2() - m.square1()) == 16)
        fen += " " + Algebraic::square((m.square1() + m.square2()) / 2) + " ";
    else
        fen += " - ";

    fen += std::to_string(gameLen) + " " + std::to_string(gameLen / 2);

    return fen;
}

// checks whether the current position has occurred twice (excluding current position) for 3 move repetition
bool Board::occursThrice(Zobrist z) {
    int32_t count = 0;
    uint8_t n = repetition.back().size();
    for (int32_t i = n - 1; i >= 0; i--) {
        if (repetition.back()[i] == z)
            count++;
        if (count == 3)
            return true;
    }
    return false;
}

// returns the std::string representation of the board based off this.sqs
std::string Board::print() {
    std::string s = "  ";
    for (int32_t i = 7; i >= 0; i--) {
        for (int32_t j = 0; j < 8; j++) {
            s += sqs[i * 8 + j].letter();
            s += ' ';
        }
        s += "\n  ";
    }
    return s + '\n';
}

// prints all bitboards
std::string Board::printBitboards(uint8_t i) {
    std::string s;
    if (i != 8)
        s += bitbs[i].print();
    else
        for (uint8_t j = 0; j < 8; j++)
            s += bitbs[j].print();
    return s + '\n';
}

// updates board given move
bool Board::makeMove(Move m) {
    if (m.isNull()) {
        bool tr = attacked() || fiftyCount[gameLen] >= 100;
        gameLen++;
        gameHist[gameLen] = m;
        zobrist.toggleSTM();
        stm = opp(stm);
        return tr;
    }

    uint8_t sq1 = m.square1(), sq2 = m.square2();
    Piece fromPiece = sqs[sq1], toPiece = sqs[sq2];
    bitbs[fromPiece.getType()].toggle(sq1);
    bitbs[stm].moveBit(sq1, sq2);
    zobrist.toggleSquare(fromPiece, sq1);

    // in the case of a promotion, the piece on sq2 will be the promotion piece, and no the piece on sq1
    if (m.special() != PROMOTE) {
        zobrist.toggleSquare(fromPiece, sq2);
        bitbs[fromPiece.getType()].toggle(sq2);
    }
    if (m.isDoublePush(fromPiece))
        zobrist.toggleEP(col(sq2));

    Move prev = gameHist[gameLen];
    if (prev.isDoublePush(sqs[prev.square2()]))
        zobrist.toggleEP(col(prev.square2()));
    gameLen++;

    // change castling rights if enemy rook is captured
    if (toPiece.getType() == ROOK &&
        row(sq2) == opp(stm) * 7
    ) {
        if (col(sq2) == 0 && canCastle(opp(stm), 0)) {
            zobrist.toggleCastle(opp(stm), false);
            castle[opp(stm)][0] = gameLen;
        }
        if (col(sq2) == 7 && canCastle(opp(stm), 1)) {
            zobrist.toggleCastle(opp(stm), true);
            castle[opp(stm)][1] = gameLen;
        }
    }
    // change castling rights if moving rook
    if (fromPiece.getType() == ROOK && row(sq1) == stm * 7) {
        if (col(sq1) == 0 && canCastle(stm, 0)) {
            zobrist.toggleCastle(stm, false);
            castle[stm][0] = gameLen;
        } else if (col(sq1) == 7 && canCastle(stm, 1)) {
            zobrist.toggleCastle(stm, true);
            castle[stm][1] = std::min(castle[stm][1], gameLen);
        }
    }
    // change castling rights if moving king
    if (fromPiece.getType() == KING) {
        if (canCastle(stm, 0)) {
            zobrist.toggleCastle(stm, false);
            castle[stm][0] = gameLen;
        }
        if (canCastle(stm, 1)) {
            zobrist.toggleCastle(stm, true);
            castle[stm][1] = std::min(castle[stm][1], gameLen);
        }
    }

    if (toPiece.getType() == EMPTY && fromPiece.getType() != PAWN)
        fiftyCount[gameLen] = fiftyCount[gameLen-1] + 1;
    else {
        fiftyCount[gameLen] = 0;
        repetition.emplace_back(0);
    }

    captured[gameLen] = toPiece;
    gameHist[gameLen] = m;
    sqs[sq2] = sqs[sq1];
    sqs[sq1] = Piece(EMPTY, WHITE);

    if (toPiece.getType() != EMPTY) {
        bitbs[toPiece.getType()].toggle(sq2);
        bitbs[opp(stm)].toggle(m.square2());
        zobrist.toggleSquare(toPiece, sq2);
        phase -= phaseInc[toPiece.getType() - 2];
    }

    switch (m.special()) {
        case PROMOTE: {
            bitbs[m.promotion()].toggle(sq2);
            sqs[sq2] = Piece(m.promotion(), stm);
            zobrist.toggleSquare({m.promotion(), stm}, sq2);
            phase += phaseInc[m.promotion() - 2];
            break;
        }
        case EP: {
            bitbs[opp(stm)].toggle((stm ? sq2 + 8 : sq2 - 8));
            bitbs[PAWN].toggle((stm ? sq2 + 8 : sq2 - 8));
            sqs[stm ? sq2 + 8 : sq2 - 8] = Piece(EMPTY, WHITE);
            break;
        }
        case CASTLE: {
            int32_t rookInit = stm * 56 + (sq1 > sq2 ? 0 : 7);
            bitbs[ROOK].moveBit((sq1 + sq2) / 2, rookInit);
            bitbs[stm].moveBit((sq1 + sq2) / 2, rookInit);
            sqs[rookInit] = Piece(EMPTY, WHITE);
            sqs[(sq1 + sq2) / 2] = Piece(ROOK, stm);
            break;
        }
    }

    bool tr = attacked() || occursThrice(zobrist) || fiftyCount[gameLen] >= 100;
    zobrist.toggleSTM();
    stm = opp(stm);
    repetition.back().push_back(zobrist);
    return tr;
}

// undoes last move
void Board::unmakeMove() {
    Move m = gameHist[gameLen];
    if (m.isNull()) {
        stm = opp(stm);
        gameLen--;
        zobrist.toggleSTM();
        return;
    }
    uint8_t sq1 = m.square1(), sq2 = m.square2();
    Piece capt = captured[gameLen];
    stm = opp(stm);
    zobrist.toggleSTM();
    Piece fromPiece = m.special() == PROMOTE ? Piece(PAWN, opp(stm)) : sqs[sq2];
    bitbs[fromPiece.getType()].toggle(sq1);
    bitbs[stm].moveBit(sq2, sq1);

    if (capt.getType() != EMPTY) {
        bitbs[capt.getType()].toggle(sq2);
        bitbs[opp(stm)].toggle(sq2);
        zobrist.toggleSquare(capt, sq2);
        phase += phaseInc[capt.getType() - 2];
    }

    if (m.special() != PROMOTE) {
        zobrist.toggleSquare(fromPiece, sq2);
        bitbs[fromPiece.getType()].toggle(sq2);
        sqs[sq1] = fromPiece;
    }
    zobrist.toggleSquare(fromPiece, sq1);
    if (abs(sq1 - sq2) == 16 && fromPiece.getType() == PAWN)
        zobrist.toggleEP(col(sq2));
    sqs[sq2] = capt;

    int8_t prevsq1 = gameHist[gameLen - 1].square1(), prevsq2 = gameHist[gameLen - 1].square2();
    bool epPawnCaptured = (sq2 == prevsq2 && captured[gameLen].getType() == PAWN) || m.special() == EP;
    if (abs(prevsq1 - prevsq2) == 16 && (sqs[prevsq2].getType() == PAWN || epPawnCaptured))
        zobrist.toggleEP(col(prevsq2));

    // special cases
    switch (m.special()) {
        case PROMOTE: {
            zobrist.toggleSquare({m.promotion(), stm}, sq2);
            bitbs[m.promotion()].toggle(sq2);
            sqs[sq1] = {PAWN, stm};
            phase -= phaseInc[m.promotion()-2];
            break;
        }
        case CASTLE: {
            int32_t dir = sq2 > sq1 ? 1 : -1, rookInit = stm * 56 + (sq1 > sq2 ? 0 : 7);
            bitbs[ROOK].moveBit(sq1+dir, rookInit);
            bitbs[stm].moveBit(sq1+dir, rookInit);
            sqs[rookInit] = {ROOK, stm};
            sqs[sq1 + dir] = {EMPTY, WHITE};
            break;
        }
        case EP: {
            uint8_t capturedSquare = stm ? sq2 + 8 : sq2 - 8;
            bitbs[PAWN].toggle(capturedSquare);
            bitbs[opp(stm)].toggle(capturedSquare);
            sqs[capturedSquare] = {PAWN, opp(stm)};
            break;
        }
    }
    repetition.back().pop_back();
    if (repetition.back().empty())
        repetition.pop_back();
    for (int32_t i = 0; i < 2; i++) {
        for (int32_t j = 0; j < 2; j++) {
            if (castle[i][j] == gameLen) {
                castle[i][j] = CANCASTLE;
                zobrist.toggleCastle(static_cast<Color>(i), j);
            }
        }
    }
    gameLen--;
}

void Board::pushMove(Move m) {
    if (!legal) {
        moves.push(m);
        return;
    }
    if (!makeMove(m))
        moves.push(m);
    unmakeMove();
}

std::pair<Bitboard, uint8_t> Board::genAttacks(Piece p, uint8_t sq, bool lva){
    Bitboard attacks = {p, sq, bitbs[WHITE] | bitbs[BLACK]};

    if (!lva)
        attacks.remove(bitbs[p.getColor()]);

    Bitboard canCapture = bitbs[opp(p.getColor())];
    Move prev = gameHist[gameLen];
    uint8_t ep = 64;
    if (prev.isDoublePush(sqs[prev.square2()])) {
        ep = (prev.square2() + prev.square1()) / 2;
        canCapture |= 1ULL << ep;
    }
    if (quiesce || p.getType() == PAWN && !lva)
        attacks &= canCapture;

    if (p.getType() == PAWN && !quiesce && !lva){
        int8_t shift = stm ? -8 : 8;
        if (sqs[sq+shift].getType() == EMPTY){
            attacks.toggle(sq+shift);
            if (row(sq) == (stm ? 6 : 1) && sqs[sq+shift*2].getType() == EMPTY)
                attacks.toggle(sq+shift*2);
        }
    }

    return {attacks, ep};
}

moveList Board::genMoves(bool legal_, bool quiesce_) {
    bool dbg = (gameHist[1].raw() == 1965 && gameHist[2].raw() == 3418);
    legal = legal_;
    quiesce = quiesce_;
    pins[0] = 0;
    pins[1] = 0;
    moves = moveList();

    Bitboard pieces = bitbs[stm], attacks;
    uint8_t piece, toSquare, ep;
    MoveType special;
    while (pieces.raw()) {
        piece = pieces.poplsb();
        std::pair<Bitboard, uint8_t> result = genAttacks(sqs[piece], piece, false);
        attacks = result.first;
        ep = result.second;

        while (attacks.raw()) {
            toSquare = attacks.poplsb();
            special = NONE;
            if (sqs[piece].getType() == PAWN){
                if (toSquare == ep)
                    special = EP;
                else if (row(toSquare) == 7 || row(toSquare) == 0)
                    special = PROMOTE;
            }
            if (special == PROMOTE)
                for (uint8_t j = ROOK; j <= QUEEN; j++)
                    pushMove({piece, toSquare, static_cast<PieceType>(j), special});
            else pushMove({piece, toSquare, ROOK, special});
        }

        genCastle(piece);
    }
    legal = false;
    quiesce = false;
    return moves;
}

void Board::genCastle(uint8_t piece) {
    if (sqs[piece].getType() == KING && !attacked(piece)) {
        Bitboard homeRank = (bitbs[WHITE] | bitbs[BLACK]) >> (stm * 56);
        if (canCastle(stm, 1))
            if (!attacked(piece + 1) && !attacked(piece + 2) && (homeRank & KINGSIDE).raw() == 0)
                pushMove({piece, static_cast<uint8_t>(piece + 2), ROOK, CASTLE});
        if (canCastle(stm, 0))
            if (!attacked(piece - 1) && !attacked(piece - 2) && (homeRank & QUEENSIDE).raw() == 0)
                pushMove({piece, static_cast<uint8_t>(piece - 2), ROOK, CASTLE});
    }
}

// for each piece, find the attack set of a piece on sq
// if intersecting this with the corresponding enemy piece yileds a non-zero, return true
// sq defaults to kings square
bool Board::attacked(uint8_t sq) {
    if (sq == 64)
        sq = (bitbs[KING] & bitbs[stm]).poplsb(false);
    for (uint8_t i = PAWN; i <= KING; i++){
        Bitboard attacks = {Piece(static_cast<PieceType>(i), stm), sq, bitbs[WHITE] | bitbs[BLACK]};
        if ((attacks & bitbs[opp(stm)] & bitbs[i]).raw())
            return true;
    }
    return false;
}

// finds the value of a piece by interpolating between midgame, and endgame values (from lookup table)
int32_t Board::val(uint8_t sq, bool simple) {
    PieceType piece = sqs[sq].getType();
    int32_t mg, eg;
    if (simple) {
        mg = mgval[piece-2];
        eg = egval[piece-2];
    } else {
        mg = mgpesto[piece-2][sqs[sq].getColor() ? mirror(sq) : sq];
        eg = egpesto[piece-2][sqs[sq].getColor() ? mirror(sq) : sq];
    }
    return (mg * std::min(24, (int32_t) phase) + eg * (24 - std::min(24, (int32_t) phase))) / 24;
}

// goes through all pieces, adding friendly piece values, and subtracting opponent pieces
// piece value 
int32_t Board::eval() {
    int32_t result[2] = {0, 0};
    uint8_t i;
    Bitboard pieces = bitbs[0] | bitbs[1];
    while (pieces.raw()) {
        i = pieces.poplsb();
        result[sqs[i].getColor()] += val(i);
    }
    return (result[0] - result[1]) * (stm ? -1 : 1);
}

uint8_t Board::lva(uint8_t sq, Bitboard traded, Color p) {
    for (uint8_t i = PAWN; i <= KING; i++){
        Bitboard attackers = genAttacks({static_cast<PieceType>(i), i == PAWN ? opp(p) : p}, sq, true).first
                & bitbs[p] & bitbs[i] & ~traded;
        if (attackers.raw())
            return attackers.poplsb();
    }
    return 64;
}

Move Algebraic::constructMove(std::string a, Board *b) {
    uint8_t sq1 = a[0] - 'a' + (a[1] - '1') * 8;
    uint8_t sq2 = a[2] - 'a' + (a[3] - '1') * 8;
    PieceType promote = ROOK;
    MoveType special = NONE;
    if (a.length() > 4) {
        special = PROMOTE;
        switch (a[4]) {
            case 'q': {
                promote = QUEEN;
                break;
            }
            case 'n': {
                promote = KNIGHT;
                break;
            }
            case 'r': {
                promote = ROOK;
                break;
            }
            case 'b': {
                promote = BISHOP;
                break;
            }
        }
    }
    Piece fromPiece = b->getsq(sq1), toPiece = b->getsq(sq2);
    if (fromPiece.getType() == PAWN && toPiece.getType() == EMPTY && col(sq1) - col(sq2) != 0)
        special = EP;
    if (fromPiece.getType() == KING && abs(sq1 - sq2) == 2)
        special = CASTLE;
    return {sq1, sq2, promote, special};
}

std::string Algebraic::move(Move m) {
    uint8_t sq1 = m.square1(), sq2 = m.square2(), prom = m.promotion(), spec = m.special();
    std::string s = square(sq1) + square(sq2);
    if (spec == PROMOTE)
        s += int2Letter[prom + 7];
    return s;
}

std::string Algebraic::square(uint8_t sq) {
    std::string s = {char(col(sq) + int32_t('a')), char(row(sq) + int32_t('1'))};
    return s;
}
