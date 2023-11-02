#include <vector>
#include <string>
#include <climits>
#include<iostream>
#include "board.h"
#include "constants.h"
#include "engine.h"
#include "perft.h"
#include "terminal.h"
using namespace std;

terminal::terminal(){
    string input;
    while (input != "S"){
        system("clear");
        cout << "Would you like to run perft (enter P), play a game (enter G), or stop (enter S)?\n--> ";
        cin >> input;
        if (input == "P")
            runPerft();
        else
            runGame();
    }
}

void terminal::runPerft(){
    system("clear");
    perft p;
    string _;
    cout << "\nEnter anything to continue\n--> ";
    cin >> _;
}

void terminal::runGame(){
    bool showpv;
    system("clear");
    bool running = true;
    uint16_t m;
    moveList moves;
    string input;
    cout << "What starting position would you like (enter a FEN string)? Put START for default.\n--> ";
    cin >> input;
    if (input == "START")
        input = START_FEN;
    e.b = board(input);
    system("clear");

    cout << "Would you like to show what moves the engine considers at each depth (the Principal Variation at each depth). Enter y/n.\n--> ";
    cin >> input;
    showpv = input == "y";
    system("clear");
    
    cout << "What color would you like to play? (\"w\" or \"b\")?\n--> ";
    cin >> input;

    task t;

    if (input == (e.b.player ? "w" : "b"))
        e.b.makeMove(e.getMove(t));
    while (running){
        system("clear");
        cout << message(showpv);
        setPos(5,17);
        cin >> input;
        m = shortFromAlgebraic(input, &e.b);
        e.b.makeMove(m);
        running = input == "stop" ? false : true;
        e.b.makeMove(e.getMove(t));
    }
}

// ses ANSI escape sequences
inline void terminal::setPos(int32_t x, int32_t y){
    string s = ("\33[" + to_string(y) + ";" + to_string(x) + "H");
    cout << s;
}

string terminal::message(bool showpv){
    string s = "";
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

uint16_t terminal::shortFromAlgebraic(string a, board* b){
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

string terminal::algebraicFromShort(uint16_t m){
    int32_t sq1 = square1(m), sq2 = square2(m), prom = promotion(m), spec = special(m);
    string s = {char(col(sq1)+int32_t('a')), char(row(sq1)+int32_t('1')), char(col(sq2)+int32_t('a')), char(row(sq2)+int32_t('1'))};
    if (spec == PROMOTE)
        s += int2Letter[prom+7];
    return s;
}

string terminal::printpvs(){
    string s = "";
    for (int32_t i = 1; i < e.pv.size(); i++){
        s += "    ";
        for (int32_t j = 0; j < e.pv[i].size(); j++)
            s += algebraicFromShort(e.pv[i][j]) + "(" + to_string(e.pv[i][j]) + (j != e.pv[i].size()-1 ? "), " : ") : ");
        s += to_string(e.pveval[i]);
        s += '\n';
    }
    return s;
}
