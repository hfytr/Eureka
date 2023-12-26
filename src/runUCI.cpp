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
    std::thread inputThread(&uci::takeInput,&u);
    std::thread workerThread(&uci::processInput,&u);
    workerThread.detach();
    inputThread.join();
    return 0;
}