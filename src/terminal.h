#include <string>
#include "search.h"
using namespace std;

#ifndef UI_H
#define UI_H
class terminal {
public:
    terminal();
    static uint16_t shortFromAlgebraic(string a, board* b);
    static string algebraicFromShort(uint16_t m);

private:
    engine e;
    void runGame(), runPerft();
    inline void setPos(int32_t x, int32_t y);
    string message(bool showpv), printpvs();
};
#endif