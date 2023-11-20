#include "terminal.h"
#include <iostream>
/*
 _/|
// o\
|| ._)
//__\
)___(
*/
int main(){
    engine e;
    e.b = board("4R3/8/2N2p2/K3P3/8/8/kb6/q7 b - - 0 1");
    std::cout << e.see(0,36);
    //terminal t;
}