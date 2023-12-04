#include "search.h"
#include "uci.h"
#include "board.h"
#include <queue>
#include <map>
#include <condition_variable>
#include <cstdint>
#include <mutex>
#include <iostream>

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
    const map<string,int32_t> cmdnum {{"uci",0},{"debug",1},{"isready",2},{"setoption",3},{"ucinewgame",4},{"position",5},{"go",6},{"stop",7},{"quit",8},{"printfen",9},{"printboard",10}};
    const map<string,int32_t> gotoken {{"searchmoves",0},{"ponder",-2},{"wtime",2},{"btime",3},{"winc",4},{"binc",5},{"movestogo",6},{"depth",7},{"nodes",8},{"movetime",9},{"infinite",-1}};
    vector<string> cur;
    while (true){
        cur = readCommand();
        switch(cmdnum.at(cur[0])){
            case 0:{ // uci
                cout << "id name Eureka\nid author Archim Jhunjhunwala\nuciok" << endl;
                break;
            }
            case 1:{ // debug
                e.debug = cur[1] == "on";
                break;
            }
            case 2:{ // isready
                unique_lock<mutex> lock{mx};
                condReady.wait(
                    lock,
                    [this] {return tasks.empty();}
                );
                lock.unlock();
                cout << "readyok" << endl;
                break;
            }
            case 3:{ // setoption
                break;
            }
            case 4:{ // ucinewgame
                e.tt.clear();
                break;
            }
            case 5:{ // position
                string fen;
                int32_t i;
                if (cur[1] == "startpos"){
                    fen = START_FEN;
                    i = 2;
                } else
                    for (i = 2; i < 8; i++)
                        fen += cur[i] + (i == 7 ? "" : " ");
                e.b = board(fen);
                for (i = ++i; i < cur.size(); i++)
                    e.b.makeMove(shortFromAlgebraic(cur[i], &e.b));
                break;
            }
            case 6:{ // go
                unique_lock<mutex> lock{mx};
                task t;
                string token;
                for (int32_t i = 1; i < cur.size(); i++){
                    if (gotoken.count(cur[i]) == 0){
                        switch(gotoken.at(token)){
                            case 1:{ // searchmoves
                                t.moves.push(shortFromAlgebraic(cur[i],&e.b));
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
                                t.mode = 2;
                                break;
                            }
                            case 8:{ // nodes
                                t.length = stoi(cur[i]);
                                t.mode = 1;
                                break;
                            }
                            case 9:{ // movetime
                                t.length = stoi(cur[i]);
                                t.mode = 0;
                                break;
                            }
                        }
                    }
                    else if (gotoken.at(cur[i]) > 0)
                        token = cur[i];
                    else{
                        if (cur[i] == "infinite"){
                            t.length = MAX32;
                            t.mode = 2;
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
                over = true;
                tasks.push(task());
                condWaitForTask.notify_one();
                return;
            }
            // the following cases are not "offical" UCI and are used purely for debugging
            case 9:{ // printfen
                cout << e.b.fen() << endl;
                break;
            }
            case 10:{ // printboard
                cout << e.b.toString();
                if (e.debug)
                    cout << e.b.printBB();
            }
        }
    }
}
/*
debug on
position fen 2rq1rk1/2p1npb1/1Pn1b1pp/p3p3/2NpP3/3P1NP1/PP1B1PBP/1R1Q1RK1 b - - 11 5
go depth 1

*/ 
void uci::processInput(){
    task t;
    while (true){
        unique_lock<mutex> lock{mx};
        condWaitForTask.wait(
            lock,
            [this] {return !tasks.empty();}
        );
        lock.unlock();
        t = tasks.front();
        if (!t.ponder)
            cout << ("bestmove " + algebraicFromShort(e.getMove(t))) << endl;;
        tasks.pop();
        if (tasks.empty())
            condReady.notify_one();
    }
}
