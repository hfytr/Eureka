#include <vector>
#include <string>
#include <climits>
#include <iostream>
#include "board.h"
#include "constants.h"
using namespace std;

void moveList::push(uint16_t m){
    container[len] = m;
    len++;
}

uint16_t moveList::operator[](int32_t i){
    return container[i];
}

void moveList::swap(int32_t i, int32_t j){
    uint16_t temp = container[i];
    container[i] = container[j];
    container[j] = temp;
}

// pops least significant 1. optionally toggles the bit
// uses DeBruijn sequences (https://www.chessprogramming.org/BitScan)
int32_t poplsb(uint64_t &n, bool remove){
    if (n == 0)
        return -1;
    int32_t i = DeBruijnLookup[((n & -n) * DeBruijn) >> 58];
    if (remove)
        toggle(n,i);
    return i;
}

// generates the ith blocker bitboard given an attack mask.
// used for magic number generation / initialisation ofmagic tables
uint64_t intToBlocker(int32_t n, uint64_t mask){
    uint64_t tr = 0;
    int32_t i = 0, cur;
    while (n){
        cur = poplsb(mask);
        if (n%2)
            toggle(tr, cur);
        n >>= 1;
        i++;
    }
    return tr;
}

int32_t count1s(uint64_t n){
    uint64_t m = n;
    int32_t i = 0;
    while(n){
        i++;
        poplsb(n);
    }
    return i;
}

// prints bitboard
string bin(uint64_t n){
    string rows[8];
    for (int32_t i = 0; i < 8; i++){
        for (int32_t j = 0; j < 8; j++){
            rows[i] += to_string(n%2);
            n >>= 1;
        }
        rows[i] += '\n';
    }
    string s = "";
    for (int32_t i = 7; i >= 0; i--)
        s += rows[i];
    return s+'\n';
}

// returns attack bitboard given square, whether or not to include the edge of the board (1 or 0) and blocker pieces
// blockers are treated as ENEMY pieces -> the edge can always be attacked and is sometimes unnecessary
uint64_t rookMask(int32_t sq, int32_t edge, uint64_t bb){
    uint64_t tr = 0;
    for (int32_t i = col(sq); i < 7+edge; i++){
        if (i != col(sq))
            toggle(tr, sq-col(sq)+i);
        if (bb & 1ULL << sq-col(sq)+i)
            break;
    }
    for (int32_t i = col(sq); i > 0-edge; i--){
        if (i != col(sq))
            toggle(tr, sq-col(sq)+i);
        if (bb & 1ULL << sq-col(sq)+i)
            break;
    }
    for (int32_t i = row(sq); i < 7+edge; i++){
        if (i != row(sq))
            toggle(tr, col(sq)+i*8);
        if (bb & 1ULL << col(sq)+i*8)
            break;
    }
    for (int32_t i = row(sq); i > 0-edge; i--){
        if (i != row(sq))
            toggle(tr, col(sq)+i*8);
        if (bb & 1ULL << col(sq)+i*8)
            break;
    }
    return tr;
}

uint64_t bishopMask(int32_t sq, int32_t edge, uint64_t bb){
    uint64_t tr = 0;
    for (int32_t i = 1; i < min(7+edge-row(sq),7+edge-col(sq)); i++){
        toggle(tr, (row(sq)+i)*8+col(sq)+i);
        if (bb & 1ULL << (row(sq)+i)*8+col(sq)+i)
            break;
    }
    for (int32_t i = 1; i < min(7+edge-row(sq),col(sq)+edge); i++){
        toggle(tr, (row(sq)+i)*8+col(sq)-i);
        if (bb & 1ULL << (row(sq)+i)*8+col(sq)-i)
            break;
    }
    for (int32_t i = 1; i < min(row(sq)+edge,7+edge-col(sq)); i++){
        toggle(tr, (row(sq)-i)*8+col(sq)+i);
        if (bb & 1ULL << (row(sq)-i)*8+col(sq)+i)
            break;
    }
    for (int32_t i = 1; i < min(row(sq)+edge,col(sq)+edge); i++){
        toggle(tr, (row(sq)-i)*8+col(sq)-i);
        if (bb & 1ULL << (row(sq)-i)*8+col(sq)-i)
            break;
    }
    return tr;
}

// returns attack bitboard given start square
uint64_t knightMask(int32_t i){
    int32_t dir[8][2] = {{2,1},{2,-1},{-2,1},{-2,-1},{1,2},{1,-2},{-1,2},{-1,-2}};
    uint64_t tr = 0;
    for (int32_t j = 0; j < 8; j++){
        if (dir[j][1] + row(i) >= 0 && dir[j][1] + row(i) < 8 && dir[j][0] + col(i) >= 0 && dir[j][0] + col(i) < 8)
            toggle(tr, (dir[j][1] + row(i))*8 + dir[j][0] + col(i));
    }
    return tr;
}

uint64_t kingMask(int32_t i){
    int32_t dir[8][2] = {{0,1},{1,0},{-1,0},{0,-1},{1,1},{1,-1},{-1,1},{-1,-1}};
    uint64_t tr = 0;
    for (int32_t j = 0; j < 8; j++){
        if (row(i)+dir[j][0] >= 0 && row(i)+dir[j][0] < 8 && col(i)+dir[j][1] >= 0 && col(i)+dir[j][1] < 8)
            toggle(tr, (row(i)+dir[j][0])*8 + (col(i)+dir[j][1]));
    }
    return tr;
}

// only includes captures
uint64_t pawnMask(int32_t sq, bool player){
    int32_t shift = player ? -1 : 1;
    uint64_t tr = 0;
    for (int32_t i = 0; i < 2; i += 2){
        if (col(sq) != player*7 && row(sq) != opp(player)*7)
            toggle(tr, sq + shift*7);
        if (col(sq) != opp(player)*7 && row(sq) != opp(player)*7)
            toggle(tr, sq + shift*9);
    }
    return tr;
}

uint16_t shortFromAlgebraic(string a, board* b){
    if (a == "stop")
        return 0;

    int32_t sq1 = int32_t(a[0])-int32_t('a') + (int32_t(a[1])-int32_t('1'))*8, sq2 = int32_t(a[2])-int32_t('a') + (int32_t(a[3])-int32_t('1'))*8, prom = 0, spec = 0;
    if (a.length() > 4){
        spec = PROMOTE;
        switch (a[4]) {
            case 'q':{
                prom = 3;
                break;
            }
            case 'n':{
                prom = 1;
                break;
            }
            case 'r':{
                prom = 0;
                break;
            }
            case 'b':{
                prom = 2;
                break;
            }
        }
    }
    int32_t fromPiece = pType(b->sqs[sq1]), toPiece = pType(b->sqs[sq2]);
    if (fromPiece == 1 && toPiece == 0 && col(sq1) - col(sq2) != 0)
        spec = EP;
    if (fromPiece == 6 && abs(sq1-sq2) == 2)
        spec = CASTLE;
    return getShort(sq1,sq2,prom,spec);
}

string algebraicFromShort(uint16_t m){
    int32_t sq1 = square1(m), sq2 = square2(m), prom = promotion(m), spec = special(m);
    string s = {char(col(sq1)+int32_t('a')), char(row(sq1)+int32_t('1')), char(col(sq2)+int32_t('a')), char(row(sq2)+int32_t('1'))};
    if (spec == PROMOTE)
        s += int2Letter[prom+7];
    return s;
}

// FEN notation: https://en.wikipedia.org/wiki/Forsyth–Edwards_Notation
board::board(string fen){
    // maps letter to corresponding piece number
    // 1 (b) -> 4, 10(k) -> 6
    int32_t posToNum[26] = {0,4,0,0,0,0,0,0,0,0,6,0,0,3,0,1,5,2,0,0,0,0,0,0,0,0};
    int32_t i=0, row=7, col=0;
    while (fen[i] != ' '){
        int32_t ascii = int32_t(fen[i]);
        if (ascii == int32_t('/') || col==8){
            row--; col=-1;
        } 
        else if (ascii < int32_t('9'))
            col += ascii - int32_t('0') - 1;

        // capital letters -> white piece
        else if (ascii < int32_t('Z')){
            int32_t piece = posToNum[ascii-int32_t('A')];
            sqs[row*8+col] = piece;
            phase += phaseInc[pType(piece)-1];
            toggle(bitbs[0][pType(piece)], row*8+col);
        }
        // lowercase letters -> black piece 
        else {
            int32_t piece = posToNum[ascii-int32_t('a')] + 7;
            sqs[row*8+col] = piece;
            phase += phaseInc[pType(piece)-1];
            toggle(bitbs[1][pType(piece)], row*8+col);
        }
        i++; col++;
    }
    i++; player = fen[i] == 'w' ? 0 : 1;
    i += 2; int32_t j = 0;
    const int32_t k = i;
    string castleChar = "KQkq";
    for (j = 0; j < 2; j++){
        for (int32_t k = 0; k < 2; k++){
            if (fen[i] != castleChar[j*2+k])
                castle[j][opp(k)] = -1;
            if (canCastle(j,opp(k)))
                i++;
        }
    }
    i++;
    // to handle ep, sets the first move as being the fen specified double pawn push
    if (fen[i] != '-'){
        int32_t sq = (int32_t(fen[i+1])-int32_t('1'))*8 + int32_t(fen[i])-int32_t('a');
        if (player)
            gameHist[0] = getShort(sq-8,sq+8,0,0);
        else
            gameHist[0] = getShort(sq+8,sq-8,0,0);
    } i += 2;
    fiftyCount = (int32_t(fen[i])-int32_t('0'))*10 + fen[i+1] != ' ' ? int32_t(fen[i+1])-int32_t('0') : 0;
    bitbs[0][0] = 0; bitbs[1][0] = 0;
    for (int32_t i = 0; i < 2; i++)
        for (int32_t j = 1; j < 7; j++)
            bitbs[i][0] |= bitbs[i][j];
    
    // initialise rook/bishup lookup tables
    for (int32_t i = 0; i < 64; i++){
        uint64_t mask = rookMasks[i];
        int32_t n = count1s(mask);
        for (int32_t j = 0; j < 1 << n; j++){
            uint64_t bb = intToBlocker(j, mask);
            int32_t ind = index(bb,rookMagics[i],rookShift[i]);
            blockedRookMasks[i][ind] = rookMask(i,1,bb);
        }
        mask = bishopMasks[i];
        n = count1s(mask);
        for (int32_t j = 0; j < 1 << n; j++){
            uint64_t bb = intToBlocker(j, mask);
            int32_t ind = index(bb,bishopMagics[i],bishopShift[i]);
            blockedBishopMasks[i][ind] = bishopMask(i,1,bb);
        }
    }

    // initialise piece square tables: adds the base piece value to the square value for each piece / game phase
    for (int32_t i = 0; i < 6; i++){
        for (int32_t j = 0; j < 64; j++){
            mgpesto[i][j] = mgval[i] + mgpestoPre[i][j^56];
            egpesto[i][j] = egval[i] + egpestoPre[i][j^56];
        }
    }
    int32_t piece,sq;
    uint64_t pieces = bitbs[0][0] | bitbs[0][1];
    while (pieces){
        int32_t sq = poplsb(pieces);
        piece = pType(sqs[sq])-1 + 6*sqs[sq]/7;
        zobrist ^= zrn[piece*64+sq];
    }
    if (player)
        zobrist ^= zrn[ZPLAYER];
    if (canCastle(0,0))
        zobrist ^= zrn[ZCASTLE];
    if (canCastle(0,1))
        zobrist ^= zrn[ZCASTLE+1];
    if (canCastle(1,0))
        zobrist ^= zrn[ZCASTLE+2];
    if (canCastle(1,1))
        zobrist ^= zrn[ZCASTLE+3];
    uint16_t m = bitbs[player][6];
    if (abs(square1(m)-square2(m)) == 16 && pType(sqs[square2(m)]) == 1)
        zobrist ^= zrn[ZEP+square2(m)%7];
    repetition.push_back(vector<uint64_t> (0));
    repetition.back().push_back(zobrist);
}

bool board::canCastle(int32_t i, int32_t j){
    return castle[i][j] == MAX32;
}

// checks whether the current position has occurred twice (excluding current position) for 3 move repetition
bool board::occursTwice(uint64_t a){
    int32_t count = 0;
    int32_t n = repetition.back().size();
    for (int32_t i = n-1; i >= 0; i--){
        if (repetition.back()[i] == a)
            count++;
        if (count == 2)
            return true;
    }
    return false;
}

// returns the string representation of the board based off this.sqs
string board::toString(){
    string s = "  ";
    for (int32_t i = 7; i >= 0; i--){
        for (int32_t j = 0; j < 8; j++){
            s += int2Letter[sqs[i*8+j]];
            s += ' ';
        }
        s += "\n  ";
    }
    return s+'\n';
}

// prints all bitboards
string board::printBB(){
    string s;
    for (int32_t i = 0; i < 14; i++)
        s += bin(bitbs[pCol(i)][pType(i)]);
    return s+'\n';
}

// updates board given move
bool board::makeMove(uint16_t m){
    int32_t sq1 = square1(m), sq2 = square2(m), prom = promotion(m), spec = special(m);
    int32_t fromPiece = sqs[sq1],  toPiece = sqs[sq2];
    toggle(bitbs[pCol(fromPiece)][pType(fromPiece)],sq1);
    moveBit(bitbs[pCol(fromPiece)][0],sq1,sq2);
    // in the case of a promotion, the piece on sq2 will be the promotion piece, and no the piece on sq1
    if (spec != PROMOTE){
        zobrist ^= zrn[(pType(fromPiece)-1 + pCol(fromPiece)*6)*64+sq2];
        toggle(bitbs[pCol(fromPiece)][pType(fromPiece)],sq2);
    }
    zobrist ^= zrn[(pType(fromPiece)-1 + pCol(fromPiece)*6)*64+sq1];
    if (abs(sq1-sq2) == 16 && pType(fromPiece) == 1)
        zobrist ^= zrn[ZEP+square2(m)%7];
    gameLen++;
    // change castling rights if enemy rook is captured
    if (pType(toPiece) == 2 && row(sq2) == opp(player)*7){
        if (col(sq2) == 0){
            if (canCastle(opp(player),0))
                zobrist ^= zrn[ZCASTLE + opp(player)*2];
            castle[opp(player)][0] = min(castle[opp(player)][0], gameLen);
        }
        if (col(sq2) == 7){
            if (canCastle(opp(player),1))
                zobrist ^= zrn[ZCASTLE + opp(player)*2 + 1];
            castle[opp(player)][1] = min(castle[opp(player)][1], gameLen);
        }
    }
    // change castling rights if moving rook
    if (pType(fromPiece) == 2 && row(sq1) == player*7){
        if (col(sq1) == 0){
            if (canCastle(player,0))
                zobrist ^= zrn[ZCASTLE + player*2];
            castle[player][0] = min(castle[player][0], gameLen);
        }
        else if (col(sq1) == 7){
            if (canCastle(player,1))
                zobrist ^= zrn[ZCASTLE + player*2 + 1];
            castle[player][1] = min(castle[player][1], gameLen);
        }
    }
    // change castling rights if moving king
    if (pType(fromPiece) == 6){
        if (canCastle(player,0))
            zobrist ^= zrn[ZCASTLE + player*2];    
        castle[player][0] = min(castle[player][0], gameLen);
        if (canCastle(player,1))
            zobrist ^= zrn[ZCASTLE + player*2 + 1];
        castle[player][1] = min(castle[player][1], gameLen);
    }
    if (toPiece == 0 && pType(fromPiece) != 1)
        fiftyCount++;
    else{
        fiftyCount = 0;
        repetition.push_back(vector<uint64_t> (0));
    }
    captured[gameLen] = toPiece;
    gameHist[gameLen] = m;
    sqs[sq2] = sqs[sq1]; sqs[sq1] = 0;
    // captures
    if (toPiece != 0){
        toggle(bitbs[pCol(toPiece)][pType(toPiece)],sq2);
        toggle(bitbs[pCol(toPiece)][0],sq2);
        zobrist ^= zrn[(pType(toPiece)-1 + pCol(toPiece)*6)*64+sq2];
        phase -= phaseInc[pType(toPiece)-1];
    }
    // special cases
    switch(spec){
        case PROMOTE : {
            toggle(bitbs[player][prom],sq2);
            sqs[sq2] = prom + player*7;
            zobrist ^= zrn[(prom-1+player*6)*64+sq2];
            phase += phaseInc[prom-1] - 1;
            break;
        }
        case EP: {
            toggle(bitbs[opp(player)][1], (player ? sq2+8 : sq2-8));
            toggle(bitbs[opp(player)][0], (player ? sq2+8 : sq2-8));
            sqs[player ? sq2+8 : sq2-8] = 0;
            break;
        }
        case CASTLE: {
            int32_t rookInit = player*56 + (sq1 > sq2 ? 0 : 7);
            moveBit(bitbs[player][2], (sq1+sq2)/2, rookInit);
            moveBit(bitbs[player][0], (sq1+sq2)/2, rookInit);
            sqs[rookInit] = 0; sqs[(sq1+sq2)/2] = 2 + player*7;
            break;
        }
    }
    bool tr = attacked() || occursTwice(zobrist) || fiftyCount == 50;
    player = opp(player);
    zobrist ^= zrn[ZPLAYER];
    repetition.back().push_back(zobrist);
    return tr;
}

// undoes last move
void board::unmakeMove(){
    uint16_t m = gameHist[gameLen];
    int32_t sq1 = square1(m), sq2 = square2(m), prom = promotion(m), spec = special(m), capt = captured[gameLen];
    player = opp(player);
    zobrist ^= zrn[ZPLAYER];
    int32_t fromPiece = spec == PROMOTE ? (1+7*player) : sqs[sq2];
    toggle(bitbs[player][pType(fromPiece)], sq1);
    moveBit(bitbs[player][0], sq2, sq1);
    // captures
    if (capt){
        toggle(bitbs[opp(player)][pType(capt)], sq2);
        toggle(bitbs[opp(player)][0], sq2);
        zobrist ^= zrn[(pType(capt)-1 + capt/7*6)*64+sq2];
        phase += phaseInc[pType(capt)-1];
    }
    // in the case of a promotion, the piece which should be removed from sq2 will be a pawn, and not the fromPiece
    if (spec != PROMOTE){
        zobrist ^= zrn[(pType(fromPiece)-1 + pCol(fromPiece)*6)*64+sq2];
        toggle(bitbs[player][pType(fromPiece)], sq2);
    }
    zobrist ^= zrn[(pType(fromPiece)-1 + pCol(fromPiece)*6)*64+sq1];
    if (abs(sq1-sq2) == 16 && pType(fromPiece) == 1)
        zobrist ^= zrn[ZEP+square2(m)%7];
    sqs[sq1] = fromPiece;
    sqs[sq2] = capt;
    // special cases
    switch (spec){
        case PROMOTE: {
            zobrist ^= zrn[(prom-1+player*6)*64+sq2];
            toggle(bitbs[player][prom], sq2);
            sqs[sq1] = 1 + (player ? 7 : 0);
            phase -= phaseInc[prom-1] - 1;
            break;
        }
        case CASTLE: {
            int32_t dir = sq2 > sq1 ? 1 : -1, rookInit = player*56 + (sq1 > sq2 ? 0 : 7);
            moveBit(bitbs[player][2], sq1+dir, rookInit);
            moveBit(bitbs[player][0], sq1+dir, rookInit);
            sqs[rookInit] = player*7+2;
            sqs[sq1+dir] = 0;
            break;
        }
        case EP: {
            toggle(bitbs[opp(player)][1], player ? sq2+8 : sq2-8);
            toggle(bitbs[opp(player)][0], player ? sq2+8 : sq2-8);
            sqs[player ? sq2+8 : sq2-8] = opp(player)*7 + 1;
            break;
        }
    }
    repetition.back().pop_back();
    if (repetition.back().size() == 0)
        repetition.pop_back();
    for (int32_t i = 0; i < 2; i++){
        for (int32_t j = 0; j < 2; j++){
            if (castle[i][j] == gameLen){
                castle[i][j] = MAX32;
                zobrist ^= zrn[ZCASTLE + i*2 + j];
            }
        }
    }
    if (capt == 0 && pType(fromPiece) != 1)
        fiftyCount--;
    gameLen--;
}

// functions to generate attack bitboards for the various pieces given piece square, side, and whether or not to remove friendly pieces;
uint64_t board::knightAttacks(int32_t sq, bool removeSame = true, int32_t p){
    p = p == -1 ? player : p;
    uint64_t tr = knightMasks[sq];
    remove(tr, bitbs[p][0]*removeSame);
    if (quiesce)
        tr &= bitbs[1-p][0];
    return tr;
}

uint64_t board::bishopAttacks(int32_t sq, bool removeSame = true, int32_t p, uint64_t omitBB){
    p = p == -1 ? player : p;
    uint64_t bb = bishopMasks[sq] & (bitbs[1][0] | bitbs[0][0]) & ~omitBB;
    int32_t ind = index(bb,bishopMagics[sq],bishopShift[sq]);
    uint64_t tr = blockedBishopMasks[sq][ind];
    if (quiesce)
        tr &= bitbs[1-p][0];
    remove(tr, bitbs[p][0]*removeSame);
    return tr;
}

uint64_t board::rookAttacks(int32_t sq, bool removeSame = true, int32_t p, uint64_t omitBB){
    p = p == -1 ? player : p;
    uint64_t bb = rookMasks[sq] & (bitbs[1][0] | bitbs[0][0]) & ~omitBB;
    int32_t ind = index(bb,rookMagics[sq],rookShift[sq]);
    uint64_t tr = blockedRookMasks[sq][ind];
    if (quiesce)
        tr &= bitbs[1-p][0];
    remove(tr, bitbs[p][0]*removeSame);
    return tr;
}

uint64_t board::queenAttacks(int32_t sq, bool removeSame = true, int32_t p, uint64_t omitBB){
    p = p == -1 ? player : p;
    return bishopAttacks(sq,removeSame,p,omitBB) | rookAttacks(sq,removeSame,p,omitBB);
}

uint64_t board::kingAttacks(int32_t sq, bool removeSame = true, int32_t p){
    p = p == -1 ? player : p;
    uint64_t tr = kingMasks[sq];
    remove(tr, bitbs[p][0]*removeSame);
    if (quiesce)
        tr &= bitbs[1-p][0];
    return tr;
}

// only captures
uint64_t board::pawnAttacks(int32_t sq, bool moveGen = true, bool removeSame = true, int32_t p){
    p = p == -1 ? player : p;
    uint64_t canAttack = bitbs[1-p][0];
    int32_t sq2 = square2(gameHist[gameLen]), sq1 = square1(gameHist[gameLen]);
    if (pType(sqs[sq2]) == 1 && abs(sq2-sq1) == 16 && abs(sq-sq2) == 1)
        toggle(canAttack, (sq1+sq2)/2);
    uint64_t tr = pawnMasks[p][sq] & (moveGen ? canAttack : ~0);
    remove(tr, bitbs[p][0]*removeSame);
    if (quiesce)
        tr &= bitbs[1-p][0];
    return tr;
}

void board::pushMove(uint16_t m){
    if (!legal){
        moves.push(m);
        return;
    }
    if (!makeMove(m))
        moves.push(m);
    unmakeMove();
}

// move generation
// generally speaking, gets pieceAttacks() to obtain the attack set, and pops each bit, creating and pushing the respective move
void board::genKnightMoves(){
    uint64_t knights = bitbs[player][3];
    while (knights){
        int32_t i = poplsb(knights);
        if (1ULL << i & (pins[0] | pins[1]))
            continue;
        uint64_t attacks = knightAttacks(i);
        while (attacks)
            pushMove(getShort(i,poplsb(attacks),0,0));
    }
}

void board::genRookMoves(){
    uint64_t rooks = bitbs[player][2];
    while (rooks){
        int32_t i = poplsb(rooks);
        uint64_t attacks = rookAttacks(i);
        if (1ULL << i & pins[1])
            continue;
        else if (1ULL << i & pins[0])
            attacks &= pins[0];
        while (attacks)
            pushMove(getShort(i,poplsb(attacks),0,0));
    }
}

void board::genBishopMoves(){
    uint64_t bishops = bitbs[player][4];
    while (bishops){
        int32_t i = poplsb(bishops);
        uint64_t attacks = bishopAttacks(i);
        if (1ULL << i & pins[0])
            continue;
        else if (1ULL << i & pins[1])
            attacks &= pins[1];
        while (attacks)
            pushMove(getShort(i,poplsb(attacks),0,0));
    }
}

void board::genQueenMoves(){
    uint64_t queens = bitbs[player][5];
    while (queens){
        int32_t i = poplsb(queens);
        uint64_t attacks = queenAttacks(i);
        if (1ULL << i & pins[1])
            attacks &= pins[1];
        else if (1ULL << i & pins[0])
            attacks &= pins[0];
        while (attacks)
            pushMove(getShort(i,poplsb(attacks),0,0));
    }
}

void board::genKingMoves(){
    int32_t i = poplsb(bitbs[player][6], false), sq;
    uint64_t attack = kingAttacks(i);
    uint16_t m;
    while (attack)
        pushMove(getShort(i,poplsb(attack)));
    
    if (!attacked(i)){
        if (canCastle(player,1))
            if (!attacked(i+1) && !attacked(i+2) && (bitbs[1][0] | bitbs[0][0]) << 1+opp(player)*56 >> 62 == 0)
                pushMove(getShort(i,i+2,0,CASTLE));
        if (canCastle(player,0))
            if (!attacked(i-1) && !attacked(i-2) && (bitbs[1][0] | bitbs[0][0]) << 4+opp(player)*56 >> 61 == 0)
                pushMove(getShort(i,i-2,0,CASTLE));
    }
}

void board::genPawnMoves(){
    uint64_t pawns = bitbs[player][1], attacks;
    int32_t shift = player ? -8 : 8;
    int32_t promote = player ? 0 : 7;
    int32_t countm, sq, i;
    bool ep;
    while (pawns){
        sq = poplsb(pawns);
        attacks = pawnAttacks(sq);
        if (sqs[sq+shift] == 0 && !quiesce){
            toggle(attacks, sq+shift);
            if (row(sq) != (player ? 1 : 6) && sqs[sq+2*shift] == 0 && row(sq) == (player ? 6 : 1))
                toggle(attacks, sq + 2*shift);
        }
        if (1ULL << sq & pins[1])
            attacks &= pins[1];
        else if (1ULL << sq & pins[0])
            attacks &= pins[0];
        while (attacks){
            i = poplsb(attacks);
            if (row(i) == promote){
                for(int32_t j = 0; j < 4; j++)
                    pushMove(getShort(sq,i,j,PROMOTE));
            } else {
                ep = sqs[i] == 0 && col(i) != col(sq);
                pushMove(getShort(sq,i,0,ep ? EP : 0));
            }
        }
    }
}

void board::genPinMasks(int32_t p, uint64_t traded){
    pins[0] = 0ULL;
    pins[1] = 0ULL;
    p = p == -1 ? player : p;
    // to find rook pins, treat the king as a rook and find its attack set with enemy pieces as blockers (call this a), intersecting with enemy rook/queens finds possible pinners.
    // for each pinner find its attack set with enemy king as blocker, then intersect this with a to find the "pinning ray"
    // if the ray has no opponent pieces (besides pinner), and only 1 friendly piece (besides king) it is a pin ray and it is added to pins
    // similar for bishops
    int32_t kSq = poplsb(bitbs[p][6], false), pinner;
    // kMask[i]: i = 0 for rook, 1 for cishop. is attack seet with enemy pieces as blockers
    uint64_t kMask[2];
    uint64_t bb = bitbs[opp(p)][0] & rookMasks[kSq] & ~traded;
    int32_t index = index(bb, rookMagics[kSq], rookShift[kSq]);
    kMask[0] = blockedRookMasks[kSq][index];
    bb = bitbs[opp(p)][0] & bishopMasks[kSq] & ~traded;
    index = index(bb, bishopMagics[kSq], bishopShift[kSq]);
    kMask[1] = blockedBishopMasks[kSq][index];
    
    uint64_t pinners[2] = {kMask[0] & (bitbs[opp(p)][2] & ~traded | bitbs[opp(p)][5] & ~traded), kMask[1] & (bitbs[opp(p)][4] & ~traded | bitbs[opp(p)][5] & ~traded)}, ray;

    while (pinners[0]){
        pinner = poplsb(pinners[0]);
        ray = blockedRookMasks[pinner][index(0,rookMagics[pinner],rookShift[pinner])] & kMask[0];
        if (singular(ray & bitbs[p][0] & ~traded) && (bitbs[opp(p)][0] & ray & ~traded) == 0)
            pins[0] |= ray | 1ULL << pinner;
    }

    while (pinners[1]){
        pinner = poplsb(pinners[1]);
        ray = blockedBishopMasks[pinner][index(0,bishopMagics[pinner],bishopShift[pinner])] & kMask[1];
        if (singular(ray & bitbs[p][0] & ~traded) && (bitbs[opp(p)][0] & ray & ~traded) == 0)
            pins[1] |= ray | 1ULL << pinner;
    }
}

moveList board::genMoves(bool legal_, bool quiesce_){
    legal = legal_;
    quiesce = quiesce_;
    moves = moveList();
    pins[0] = 0; pins[1] = 0;
    if (legal)
        genPinMasks();
    genPawnMoves();
    genRookMoves();
    genKnightMoves();
    genBishopMoves();
    genQueenMoves();
    genKingMoves();
    legal = false;
    quiesce = false;
    return moves;
}

// for each piece, find the attack set of a piece on sq
// if intersecting this with the corresponding enemy piece yileds a non zero, return true
// sq defaults to kings square
bool board::attacked(int32_t sq){
    if (sq == -1)
        sq = poplsb(bitbs[player][6],false);
    if (pawnAttacks(sq,false,false,player) & bitbs[opp(player)][1])
        return true;
    if (bitbs[opp(player)][3] & knightMasks[sq])
        return true;
    if (rookAttacks(sq) & (bitbs[opp(player)][2] | bitbs[opp(player)][5]))
        return true;
    if (bishopAttacks(sq) & (bitbs[opp(player)][4] | bitbs[opp(player)][5]))
        return true;
    if (kingAttacks(sq) & bitbs[opp(player)][6])
        return true;
    return false;
}

// finds the value of a piece by interpolating between midgame, and endgame values (from lookup table)
int32_t board::val(int32_t sq){
    int32_t piece = pType(sqs[sq]);
    int32_t mg = mgpesto[piece-1][pCol(sqs[sq]) ? mirror(sq) : sq];
    int32_t eg = egpesto[piece-1][pCol(sqs[sq]) ? mirror(sq) : sq];
    return (mg * min(24,phase) + eg * (24-min(24,phase)))/24;
}

// goes through all pieces, adding friendly piece values, and subtracting opponent pieces
// piece value 
int32_t board::eval(){
    int32_t result[2] = {0,0}, i;
    uint64_t pieces = bitbs[0][0] | bitbs[1][0];
    while (pieces){
        i = poplsb(pieces);
        result[pCol(sqs[i])] += val(i);
    }
    return (result[0] - result[1]) * (player ? -1 : 1);
}

int32_t board::lva(int32_t sq, uint64_t traded, int32_t p){
    genPinMasks();
    p = p == -1 ? player : p;
    uint64_t attacker = pawnAttacks(sq,false,false,opp(p)) & bitbs[p][1] & ~pins[0] & ~pins[1] & ~traded;
    if (attacker)
        return poplsb(attacker);
    attacker = knightAttacks(sq,false,p) & bitbs[p][3] & ~pins[0] & ~pins[1] & ~traded;
    if (attacker)
        return poplsb(attacker);
    attacker = bishopAttacks(sq,false,p,traded) & bitbs[p][4] & ~pins[0] & ~pins[1] & ~traded;
    if (attacker)
        return poplsb(attacker);
    attacker = rookAttacks(sq,false,p,traded) & bitbs[p][2] & ~pins[0] & ~pins[1] & ~traded;
    if (attacker)
        return poplsb(attacker);
    attacker = queenAttacks(sq,false,p,traded) & bitbs[p][5] & ~pins[0] & ~pins[1] & ~traded;
    if (attacker)
        return poplsb(attacker);
    attacker = kingAttacks(sq,false,p) & bitbs[p][6];
    if (attacker)
        return poplsb(attacker);
    return -1;
}