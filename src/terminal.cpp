#include <string>
#include <iostream>
#include "board.h"
#include "constants.h"
#include "search.h"
#include "perft.h"
#include "terminal.h"


terminal::terminal(){
    std::string input;
    while (input != "S"){
        system("clear");
        std::cout << "Would you like to run perft (enter P), play a game (enter G), or stop (enter S)?\n--> ";
        std::cin >> input;
        if (input == "P")
            runPerft();
        else
            runGame();
    }
}

void terminal::runPerft(){
    system("clear");
    perft p;
    std::string _;
    std::cout << "\nEnter anything to continue\n--> ";
    std::cin >> _;
}

void terminal::runGame(){
    bool showpv;
    system("clear");
    bool running = true;
    uint16_t m;
    std::string input;
    std::cout << "What starting position would you like (enter a FEN string)? Put START for default.\n--> ";
    std::cin >> input;
    if (input == "START")
        input = START_FEN;
    e.b = board(input);
    system("clear");

    std::cout << "Would you like to show what moves the engine considers at each depth (the Principal Variation at each depth). Enter y/n.\n--> ";
    std::cin >> input;
    showpv = input == "y";
    system("clear");
    
    std::cout << "What color would you like to play? (\"w\" or \"b\")?\n--> ";
    std::cin >> input;

    task t;

    if (input == (e.b.player ? "w" : "b"))
        e.b.makeMove(e.getMove(t));
    while (running){
        system("clear");
        std::cout << message(showpv);
        gotoInputPos();
        std::cin >> input;
        m = shortFromAlgebraic(input, &e.b);
        e.b.makeMove(m);
        running = !(input == "stop");
        e.b.makeMove(e.getMove(t));
    }
}

// ses ANSI escape sequences
inline void terminal::gotoInputPos(){
    std::string s = ("\33[" + std::to_string(17) + ";" + std::to_string(5) + "H");
    std::cout << s;
}

std::string terminal::message(bool showpv){
    std::string s;
    s += "-------- BOARD --------\n\n" + e.b.toString() + "\n-------- MOVE HISTORY --------\n    ";
    for (int32_t i = 1; i <= e.b.gameLen; i++)
        s += algebraicFromShort(e.b.gameHist[i]) + " ";
    s += "\n\n-------- INPUT NEXT MOVE (put \"stop\" to stop)--------\n--> \n\n";
    if (showpv){
        s += "-------- PRINCIPAL VARIATION --------\n";
        s += printpvs();
    }
    return s;
}

uint16_t terminal::shortFromAlgebraic(std::string a, board* b){
    if (a == "stop")
        return 0;

    int32_t sq1 = int32_t(a[0])-int32_t('a') + (int32_t(a[1])-int32_t('1'))*8, sq2 = int32_t(a[2])-int32_t('a') + (int32_t(a[3])-int32_t('1'))*8, prom = 0, spec = 0;
    if (a.length() > 4){
        spec = PROMOTE;
        switch (a[4]) {
            case 'q':{
                prom = 3;
                break;
            }
            case 'n':{
                prom = 1;
                break;
            }
            case 'r':{
                prom = 0;
                break;
            }
            case 'b':{
                prom = 2;
                break;
            }
        }
    }
    int32_t fromPiece = pType(b->sqs[sq1]), toPiece = pType(b->sqs[sq2]);
    if (fromPiece == 1 && toPiece == 0 && col(sq1) - col(sq2) != 0)
        spec = EP;
    if (fromPiece == 6 && abs(sq1-sq2) == 2)
        spec = CASTLE;
    return getShort(sq1,sq2,prom,spec);
}

std::string terminal::algebraicFromShort(uint16_t m){
    int32_t sq1 = square1(m), sq2 = square2(m), prom = promotion(m), spec = special(m);
    std::string s = {char(col(sq1)+int32_t('a')), char(row(sq1)+int32_t('1')), char(col(sq2)+int32_t('a')), char(row(sq2)+int32_t('1'))};
    if (spec == PROMOTE)
        s += int2Letter[prom+7];
    return s;
}

std::string terminal::printpvs(){
    std::string s;
    for (int32_t i = 1; i < e.pv.size(); i++){
        s += "    ";
        for (int32_t j = 0; j < e.pv[i].size(); j++)
            s += algebraicFromShort(e.pv[i][j]) + "(" + std::to_string(e.pv[i][j]) + (j != e.pv[i].size()-1 ? "), " : ") : ");
        s += std::to_string(e.pveval[i]);
        s += '\n';
    }
    return s;
}
