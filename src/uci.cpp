#include "engine.h"
#include "uci.h"
#include <queue>
#include <map>
#include <condition_variable>
#include <mutex>
#include <iostream>
using namespace std;

vector<string> uci::readCommand(){
    vector<string> tr;
    string cur;
    cin >> cur; tr.push_back(cur);
    while (cin.peek() != '\n'){
        cin >> cur;
        tr.push_back(cur);
    }
    return tr;
}

void uci::takeInput(){
    const map<string,int32_t> cmdnum {{"uci",0},{"debug",1},{"isready",2},{"setoption",3},{"ucinewgame",4},{"position",5},{"go",6},{"stop",7}};
    const map<string,int32_t> gotoken {{"searchmoves",0},{"ponder",1},{"wtime",2},{"btime",3},{"winc",4},{"binc",5},{"movestogo",6},{"depth",7},{"nodes",8},{"movetime",9},{"infinite",10}};
    vector<string> cur;
    while (true){
        cur = readCommand();
        if (cur[0] == "quit")
            return;
        switch(cmdnum.at(cur[0])){
            case 0:{ // uci
                cout << "id name Eureka 1.0\nid author Archim Jhunjhunwala\nuciok\n";
            }
            case 1:{ // debug
                e.debug = cur[1] == "on";
                break;
            }
            case 2:{ // isready
                unique_lock<std::mutex> lock{mxReady};
                condReady.wait(
                    lock,
                    [this] {return tasks.empty();}
                );
                lock.unlock();
                cout << "readyok\n";
                break;
            }
            case 3:{ // setoption
                break;
            }
            case 4:{ // ucinewgame
                e.tt.clear();
                break;
            }
            case 5:{ // positionSS
                if (cur[1] == "startpos")
                    cur[1] = START_FEN;
                e.b = board(cur[1]);
                for (int32_t i = 2; i < cur.size(); i++)
                    e.b.makeMove(shortFromAlgebraic(cur[i], &e.b));
                break;
            }
            case 6:{ // go
                task t;
                string token;
                for (int32_t i = 1; i < cur.size(); i++){
                    if (gotoken.count(cur[i]) == 1)
                        token = cur[i];
                    else {
                        switch(gotoken.at(cur[i])){
                            case 0:{
                                t.moves.push(shortFromAlgebraic(cur[i],&e.b));
                                break;
                            }
                            case 1:{
                                t.ponder = true;
                                break;
                            }
                            case 2:{
                                t.timeLeft[0] = stoi(cur[i]);
                                break;
                            }
                            case 3:{
                                t.timeLeft[1] = stoi(cur[i]);
                                break;
                            }
                            case 4:{
                                t.increment[0] = stoi(cur[i]);
                                break;
                            }
                            case 5:{
                                t.increment[1] = stoi(cur[i]);
                                break;
                            }
                            case 6:{
                                t.movestogo = stoi(cur[i]);
                                break;
                            }
                            case 7:{
                                t.length = stoi(cur[i]);
                                t.mode = 2;
                                break;
                            }
                            case 8:{
                                t.length = stoi(cur[i]);
                                t.mode = 1;
                                break;
                            }
                            case 9:{
                                t.length = stoi(cur[i]);
                                t.mode = 0;
                                break;
                            }
                            case 10:{
                                t.length = INT_MAX;
                                t.mode = 2;
                            }
                        }
                    }
                }
                tasks.push(t);
                break;
            }
            case 7:{ // stop
                e.over = true;
                break;
            }
        }
    }
}

void uci::processInput(){
    task cur;
    while (true){
        unique_lock<std::mutex> lock{mxWaitForTask};
        condWaitForTask.wait(
            lock,
            [this] {return !tasks.empty();}
        );
        lock.unlock();
        cur = tasks.front();

        tasks.pop();
    }
}