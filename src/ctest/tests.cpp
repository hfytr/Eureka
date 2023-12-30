#include "perft.h"

int main(int argc, char* argv[]){
    perft p;
    return p.runPerft(argv[1][0]-int('0'));
}