#include "uci.h"
#include <thread>
/*

     _/|
    // o\
    || ._)
    //__\
    )___(

*/
int main(){
    uci u;
    thread inputThread(&uci::takeInput,&u);
    thread workerThread(&uci::processInput,&u);
    workerThread.detach();
    inputThread.join();
    return 0;
}