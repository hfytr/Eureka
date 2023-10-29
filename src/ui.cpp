#include <vector>
#include <string>
#include <climits>
#include<iostream>
#include "board.h"
#include "data.h"
#include "engine.h"
#include "perft.h"
#include "ui.h"
using namespace std;

ui::ui(){
    setPos(10,10);
    cout << "hi";
    int foo = 0;
    /*
    bool running = true, pins = false;
    uint16_t m;
    moveList moves;
    string input;
    cout << "  What color would you like to play? (\"w\" or \"b\")?\n--->";
    cin >> input;
    input = "w";
    uint16_t engineMove;
    if (input == "b"){
        system("cls");
        engineMove = e.getMove();
        e.b.makeMove(engineMove);
    }
    while (running){
        pins = false;
        setPos(0,0);
        system("cls");
        cout << message();
        setPos(2,16);
        cin >> input;
        m = shortFromAlgebraic(input);
        e.b.makeMove(m);
        running = input == "stop" ? false : true;
        e.b.makeMove(e.getMove());
    }*/
}

void ui::setPos(int32_t x, int32_t y){
    string s = "\033[" +  to_string(y) + ";" + to_string(x);
}

string ui::message(){
    string s = "";
    s += "-------- BOARD --------\n\n" + e.b.toString() + "\n-------- MOVE HISTORY --------\n  ";
    for (int32_t i = 1; i <= e.b.gameLen; i++)
        s += algebraicFromShort(e.b.gameHist[i]) + " ";
    s += "\n\n-------- INPUT NEXT MOVE (put \"stop\" to stop)--------\n  \n\n";
    s += "-------- PRINCIPAL VARIATION --------\n";
    s += printpvs();
    return s;
}

uint16_t ui::shortFromAlgebraic(string a){
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
    int32_t fromPiece = pType(e.b.sqs[sq1]), toPiece = pType(e.b.sqs[sq2]);
    if (fromPiece == 1 && toPiece == 0 && col(sq1) - col(sq2) != 0)
        spec = EP;
    if (fromPiece == 6 && abs(sq1-sq2) == 2)
        spec = CASTLE;
    return getShort(sq1,sq2,prom,spec);
}

string ui::algebraicFromShort(uint16_t m){
    int32_t sq1 = square1(m), sq2 = square2(m), prom = promotion(m), spec = special(m);
    string s = {char(col(sq1)+int32_t('a')), char(row(sq1)+int32_t('1')), char(col(sq2)+int32_t('a')), char(row(sq2)+int32_t('1'))};
    if (spec == PROMOTE)
        s += int2Letter[prom+7];
    return s;
}

string ui::printpvs(){
    string s = "";
    for (int32_t i = 1; i < e.pv.size(); i++){
        s += "  ";
        for (int32_t j = 0; j < e.pv[i].size(); j++)
            s += algebraicFromShort(e.pv[i][j]) + "(" + to_string(e.pv[i][j]) + (j != e.pv[i].size()-1 ? "), " : ") : ");
        s += to_string(e.pveval[i]);
        s += '\n';
    }
    return s;
}
