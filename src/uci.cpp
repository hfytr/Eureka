#include "search.h"
#include "uci.h"
#include "board.h"
#include <queue>
#include <map>
#include <mutex>
#include <iostream>

std::vector<std::string> uci::readCommand(){
    std::vector<std::string> tr;
    std::string cur;
    std::cin >> cur; tr.push_back(cur);
    while (std::cin.peek() != '\n'){
        std::cin >> cur;
        tr.push_back(cur);
    }
    return tr;
}

void uci::takeInput(){
    const std::map<std::string,int32_t> cmdnum {{"uci",0},{"debug",1},{"isready",2},{"setoption",3},{"ucinewgame",4},{"position",5},{"go",6},{"stop",7},{"quit",8},{"printfen",9},{"printboard",10}};
    const std::map<std::string,int32_t> gotoken {{"searchmoves",0},{"ponder",-2},{"wtime",2},{"btime",3},{"winc",4},{"binc",5},{"movestogo",6},{"depth",7},{"nodes",8},{"movetime",9},{"infinite",-1}};
    std::vector<std::string> cur;
    while (true){
        cur = readCommand();
        switch(cmdnum.at(cur[0])){
            case 0:{ // uci
                std::cout << "id name Eureka\nid author Archim Jhunjhunwala\noption name Hash type spin default 1\noption name Move Overhead type spin default 20\nuciok" << std::endl;
                break;
            }
            case 1:{ // debug
                e.debug = cur[1] == "on";
                break;
            }
            case 2:{ // isready
                std::unique_lock<std::mutex> lock{mx};
                condReady.wait(
                    lock,
                    [this] {return tasks.empty();}
                );
                lock.unlock();
                std::cout << "readyok" << std::endl;
                break;
            }
            case 3:{ // setoption
                if (cur[2] == "Hash")
                    e.tt.resize(stoi(cur[4]));
                else if (cur[2] == "Move" && cur[3] == "Overhead")
                    e.moveOverhead = stoi(cur[5]);
                break;
            }
            case 4:{ // ucinewgame
                e.tt.clear();
                break;
            }
            case 5:{ // position
                std::string fen;
                int32_t i;
                if (cur[1] == "startpos"){
                    fen = START_FEN;
                    i = 2;
                } else
                    for (i = 2; i < 8; i++)
                        fen += cur[i] + (i == 7 ? "" : " ");
                e.b = Board(fen);
                for (i = ++i; i < cur.size(); i++)
                    e.b.makeMove(Algebraic::constructMove(cur[i], &e.b));
                break;
            }
            case 6:{ // go
                std::unique_lock<std::mutex> lock{mx};
                task t;
                std::string token;
                for (int32_t i = 1; i < cur.size(); i++){
                    if (gotoken.count(cur[i]) == 0){
                        switch(gotoken.at(token)){
                            case 1:{ // searchmoves
                                t.moves.push(Algebraic::constructMove(cur[i],&e.b));
                                break;
                            }
                            case 2:{ // wtime
                                t.timeLeft[0] = stoi(cur[i]);
                                break;
                            }
                            case 3:{ // btime
                                t.timeLeft[1] = stoi(cur[i]);
                                break;
                            }
                            case 4:{ // winc
                                t.increment[0] = stoi(cur[i]);
                                break;
                            }
                            case 5:{ // binc
                                t.increment[1] = stoi(cur[i]);
                                break;
                            }
                            case 6:{ // movestogo
                                t.movestogo = stoi(cur[i]);
                                break;
                            }
                            case 7:{ // depth
                                t.length = stoi(cur[i]);
                                t.mode = DEPTH;
                                break;
                            }
                            case 8:{ // nodes
                                t.length = stoi(cur[i]);
                                t.mode = NODES;
                                break;
                            }
                            case 9:{ // movetime
                                t.length = stoi(cur[i]);
                                t.mode = TIME;
                                break;
                            }
                        }
                    }
                    else if (gotoken.at(cur[i]) > 0)
                        token = cur[i];
                    else{
                        if (cur[i] == "infinite"){
                            t.length = CANCASTLE;
                            t.mode = DEPTH;
                        } else
                            t.ponder = true;
                    }
                }
                tasks.push(t);
                condWaitForTask.notify_one();
                break;
            }
            case 7:{ // stop
                e.forceStop = true;
                break;
            }
            case 8:{ // quit
                quit = true;
                tasks.emplace();
                condWaitForTask.notify_one();
                return;
            }
            // the following cases are not "offical" UCI and are used purely for debugging
            case 9:{ // printfen
                std::cout << e.b.fen() << std::endl;
                break;
            }
            case 10:{ // printboard
                std::cout << e.b.print();
                if (e.debug == 1)
                    std::cout << e.b.printBitboards();
            }
        }
    }
}

void uci::processInput(){
    task t;
    while (true){
        std::unique_lock<std::mutex> lock{mx};
        condWaitForTask.wait(
            lock,
            [this] {return !tasks.empty();}
        );
        lock.unlock();
        t = tasks.front();
        if (quit)
            return;
        if (!t.ponder)
            std::cout << ("bestmove " + Algebraic::move(e.getMove(t).m())) << std::endl;
        tasks.pop();
        if (tasks.empty())
            condReady.notify_one();
    }
}
