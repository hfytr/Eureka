#ifndef UCI_H
#define UCI_H
#include "search.h"
#include "board.h"
#include <queue>
#include <condition_variable>
#include <mutex>

class uci{
public:
    void processInput(), takeInput();
private:
    engine e;
    bool over;
    queue<task> tasks;
    vector<string> readCommand();
    condition_variable condWaitForTask, condReady;
    mutex mx;
};
#endif