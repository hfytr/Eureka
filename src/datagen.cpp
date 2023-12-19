#include <string>
#include <vector>
#include <fstream>
#include <iostream>
#include <deque>
#include <thread>
#include <ctime>
#include "search.h"
#include "board.h"
#include "datagen.h"

#define NUM_THREADS 14
#define NUM_FENS 34701

void datagenThread::start(const std::vector<std::string> &fens, int32_t totalPositions_){
    e.debug = 2;
    totalPositions = totalPositions_;
    int i = 0;
    while (positionsLogged <= totalPositions) {
        b = Board(fens[i]);
        processFEN();
        std::cout << positionsLogged << ' ';
        i++;
    }
}

void datagenThread::processFEN(int32_t depth){
    if (depth == 0){
        runGame();
        return;
    }

    moveList moves = b.genMoves(true, false);
    for (int32_t i = 0; i < moves.len(); i++){
        bool illegal = b.makeMove(moves[i]);
        if (illegal){
            b.unmakeMove();
            continue;
        }

        processFEN(depth-1);
        b.unmakeMove();
        if (positionsLogged >= totalPositions)
            return;
    }
}

void datagenThread::runGame(){
    bool over = false;
    task t = {0, 0, 0, 0, 0, NODES, 5000, false};
    std::deque<int32_t> history;
    Board gameBoard = Board(b.fen());
    while (!over){
        e.b = Board(gameBoard.fen());
        xMove best = e.getMove(t);

        history.push_back(best.eval);
        if (history.size() == 3){
            history.pop_front();
            if (abs(history[0]) >= 900 && abs(history[1]) >= 900 && abs(history[2]) >= 900)
                return;
        }

        gameBoard.makeMove(best.m());
        entries.emplace_back(e.b.getZobrist().getBits(), best.eval);
        positionsLogged++;
        over = gameBoard.isOver() || positionsLogged >= totalPositions;
    }
}

void datagen::logEntry(entry e, std::ofstream &fout){
    unsigned char bytes[102];
    for (int32_t i = 0; i < 72; i++)
        for (int32_t j = 0; j < 8; j++)
            bytes[i] |= e.first[i*8+j] << j;
    for (int32_t i = 0; i < 4; i++){
        bytes[i+72] = e.second & 0xff;
        e.second >>=  8;
    }
    fout.write(reinterpret_cast<const char *>(bytes), 102);
    std::cout << e.second;
}

datagen::datagen(){
    std::cout << "How many positions?";
    int32_t numPositions;
    std::cin >> numPositions;
    std::ifstream fin("./main/books/book.epd");
    std::vector<std::string> threadFens[NUM_THREADS];
    int32_t threadNum = 0;
    for (int i = 0; i < NUM_FENS; i++){
        std::string s;
        std::getline(fin, s);
        s = s.substr(0, s.size()-1);
        if (i != 0)
            threadFens[threadNum].push_back(s);
        if (i % (NUM_FENS/NUM_THREADS) == 0 && i != 0 && threadNum != NUM_THREADS-1)
            threadNum++;
    }

    std::vector<datagenThread> threadObjects (NUM_THREADS);
    std::vector<std::thread> threads (NUM_THREADS);
    for (int32_t i = 0; i < NUM_THREADS; i++)
        threads[i] = std::thread([=, &threadObjects](){
            threadObjects[i].start(threadFens[i], numPositions / NUM_THREADS); // frankly dont care that final positions are nver used; won't be anyway
        });

    for (int32_t i = 0; i < NUM_THREADS; i++)
        threads[i].join();

    std::time_t t = std::time(0);
    std::tm now = *std::localtime(&t);
    std::string date = std::to_string(now.tm_year + 1900) + "-" +
            std::to_string(now.tm_mon + 1) + "-" +
            std::to_string(now.tm_mday) + "-";
    std::ofstream fout = std::ofstream("./training/data/" + date + std::to_string(numPositions) + ".training");
    for (int32_t i = 0; i < NUM_THREADS; i++)
        for (int32_t j = 0; j < threadObjects[i].entries.size(); j++)
            logEntry(threadObjects[i].entries[j], fout);
}