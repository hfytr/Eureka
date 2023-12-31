#ifndef CONSTANTS_H
#define CONSTANTS_H
#include <cstdint>

#define ZPLAYER 768
#define ZCASTLE 769
#define ZEP 773
#define DEFAULTTTSIZE 1
#define START_FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
#define MIN32 (INT_MIN+1)
#define MAX32 (INT_MAX)
#define CANCASTLE 65535
#define CANTCASTLE 65534
#define GOOD_CAPTURE (MAX32 - 2000000)
#define FAIL_HIGH 0
#define FAIL_LOW 1
#define PV_NODE 2
#define CHECKMATE (MAX32-1)
#define CHECKMATED (MIN32+1)
#define NUM_BUCKETS 4
#define NULLMOVE 0
#define REDUCE_AFTER 4
#define SEE_CUTOFF (-1000)
#define QUEENSIDE 0xe
#define KINGSIDE 0x60

extern int32_t DeBruijnLookup[64];
extern int32_t rookShift[64];
extern int32_t bishopShift[64];
extern uint64_t DeBruijn;
extern uint64_t bishopMagics[64];
extern uint64_t rookMagics[64];
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