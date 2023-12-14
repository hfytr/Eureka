#include <string>
#include "search.h"


#ifndef UI_H
#define UI_H
class terminal {
public:
    terminal();
    static uint16_t shortFromAlgebraic(std::string a, board* b);
    static std::string algebraicFromShort(uint16_t m);

private:
    engine e;
    void runGame();
    inline void gotoInputPos();
    std::string message(bool showpv);
};
#endif
