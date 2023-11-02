#ifndef UCI_H
#define UCI_H
#include "engine.h"
#include <queue>
#include <condition_variable>
#include <mutex>

class uci{
public:
    void processInput(), takeInput();
private:
    engine e;
    queue<task> tasks;
    vector<string> readCommand();
    condition_variable condWaitForTask, condReady;
    mutex mxWaitForTask, mxReady;
};
#endif