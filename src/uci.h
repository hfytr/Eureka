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
    bool quit = false;
    std::queue<task> tasks;
    std::vector<std::string> readCommand();
    std::condition_variable condWaitForTask, condReady;
    std::mutex mx;
};
#endif