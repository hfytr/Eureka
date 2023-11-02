#include <cstdint>
#ifndef constants_H
#define constants_H
#define PROMOTE 1
#define EP 2
#define CASTLE 3
#define ZPLAYER 768
#define ZCASTLE 769
#define ZEP 773
#define TTSIZE 100000
#define AGETOLERANCE 5
#define MINMVVLVA INT_MAX - 2000000
#define START_FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
#define UCIQUIT 0

extern int32_t DeBruijnLookup[64];
extern int32_t rookShift[64];
extern int32_t bishopShift[64];
extern uint64_t DeBruijn;
extern uint64_t bishopMagics[64];
extern uint64_t rookMagics[64];
extern uint64_t knightMasks[64];
extern uint64_t kingMasks[64];
extern uint64_t pawnMasks[2][64] ;
extern uint64_t rookMasks[64];
extern uint64_t bishopMasks[64];
extern uint64_t blockedRookMasks[64][4096];
extern uint64_t blockedBishopMasks[64][4096];
extern int32_t mgpawnPesto[64];
extern int32_t egpawnPesto[64];
extern int32_t mgknightPesto[64];
extern int32_t egknightPesto[64];
extern int32_t mgbishopPesto[64];
extern int32_t egbishopPesto[64];
extern int32_t mgrookPesto[64];
extern int32_t egrookPesto[64];
extern int32_t mgqueenPesto[64];
extern int32_t egqueenPesto[64];
extern int32_t mgkingPesto[64];
extern int32_t egkingPesto[64];
extern int32_t* mgpestoPre[6];
extern int32_t* egpestoPre[6];
extern int32_t mgpesto[6][64];
extern int32_t egpesto[6][64];
extern int32_t phaseInc[6];
extern int32_t mgval[6];
extern int32_t egval[6];
extern char int2Letter[14];
extern uint64_t zrn[781];
#endif