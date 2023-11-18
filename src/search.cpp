#include <vector>
#include <map>
#include <string>
#include <chrono>
#include <climits>
#include <iostream>
#include "board.h"
#include "constants.h"
#include "search.h"
using namespace std;
#define print(b) (cout << b.toString() << b.printBB())

int32_t engine::initialTime(){
    int32_t optimalTime;
    if (t.movestogo > 0)
        optimalTime = t.increment[b.player]/2 + t.timeLeft[b.player]/t.movestogo;
    else
        optimalTime = t.increment[b.player]/2 + t.timeLeft[b.player]/20;
    return min(optimalTime, t.timeLeft[b.player]-20);
}

/// @brief scores a move in following order 1) best move from last pass of ID (if node is pv node), 2) transposition table hit, 3) captures sorted by MVVLVA (most valuable victim, least valuable attacker), 4) killer moves, 5) butterfly score
/// butterfly score is meant to indicate the strength of positional moves. they are updated whenever a beta cutoff is caused by a non capture/promotion and are increased by depth^2 whenever such a beta cutoff occurs. depth^2 lowers impact of low-depth nodes
/// @param m the move to score
/// @param depth the depth of search (scoreMove always called by negaMac/search)
/// @param killer killer moves
/// @param ispv whether or not the current node is a pv node
/// @return the score of a move (arbitrary number, used relatively)
int32_t engine::scoreMove(uint16_t m, int32_t depth, pair<uint16_t,uint16_t> killer, bool ispv){
    if (tt.get(b.zobrist).m == m)
        return MAX32-1;

    bool capture = b.sqs[square2(m)] != 0;
    
    if (capture)
        return 1000*b.val(square2(m)) + (1000-b.val(square1(m))) + MINMVVLVA;
    
    if (killer.first == m || killer.second == m)
        return MINMVVLVA-1;
    
    return min(butterfly[b.player][square1(m)][square2(m)],MINMVVLVA-2);
}

bool engine::checkOver(){
    if (forceStop)
        return true;
    switch (t.mode){
        case 0:{    
            auto passed = steady_clock::now()-start;
            return duration_cast<chrono::milliseconds>(passed).count() > t.length;
        }
        case 1:
            return nodes > t.length;
        case 2:
            return fullDepth > t.length;
    }
    return true;
}

/// @brief checks whether current board is line to debug
/// @return bool - true if is debug line else false
bool engine::isDbgLine(){
    string dbgLine[7] = {};
    int32_t i;
    for (i = 0; i < b.gameLen+1; i++)
        if (i == b.gameLen || dbgLine[i] != algebraicFromShort(b.gameHist[i+1]))
            break;
    if (i == b.gameLen && fullDepth == 7){
        cout << b.toString();
        return true;
    }
    return false;
}

/// @brief performs search at given depth + quiescent search; quiescent search is infinite depth search which considers only captures/promotions to prevent the horizon effect
/// @param depth the depth to search
/// @param alpha current players best option - can be any distance up the tree
/// @param beta opposing players best option - also can be any distance up the tree; if any move is greater than beta, we immediately return, as the opposing player has a better option, and would simply make a better move higher in the tree, rendering current results useless; moves which cause a beta cutoff "killer moves" are likely to also be good no matter the opponents reply. we search these first in "cousin" nodes (nodes which share a grandparent)
/// @param killerOpp stores killer moves for opponent and is passed too, and used in child nodes
/// @param killer killer moves for current posisiton and are searched early
/// @param parentpv passed by the parent node to store current nodes pv
/// @param ispv whether or not the current node is in the pv of the last pass of ID
/// @return the score of the position from current players perspective, NOT the actual best move
int32_t engine::negamax(int32_t depth, int32_t alpha, int32_t beta, pair<uint16_t,uint16_t> killerOpp, pair<uint16_t,uint16_t> &killer, vector<uint16_t> &parentpv, bool ispv){
    // uint64_t pawn = b.bitbs[1][1] & 0x404040404040;
    // int32_t rank = poplsb(pawn)/8;
    // if (rank == 5-(b.gameLen+1)/2 && rank == 3 && b.player == 1)
    //     cout << b.toString();
    selDepth = min(depth, selDepth);
    over = checkOver();
    nodes++;
    // return TT hits with entry.depth > depth
    TTnode entry = tt.get(b.zobrist);
    if (entry.m != 0 && entry.depth >= depth && (entry.type == PV_NODE || (entry.type == CUT_NODE && entry.eval >= beta) || (entry.type == ALL_NODE && entry.eval <= alpha)))
        return entry.eval;

    moveList moves = b.genMoves(false, depth <= 0);
    TTnode ttEntry = TTnode(b.zobrist,0,0,max(depth,0),ALL_NODE);
    int32_t j , i, curInd, cur;
    xMove best;
    uint16_t m;
    vector<int32_t> score;
    score.resize(moves.len);
    pair<uint16_t,uint16_t> killerNext;
    vector<uint16_t> childpv;
    bool illegal, gameOver = depth > 0;

    // it is (mostly) possible to not make any captures and instead "stand pat" so this is alphas initial value
    // only applicable to quiescent search (when depth <= 0)
    if (depth <= 0){
        best.eval = b.eval();
        if (alpha < best.eval)
            alpha = best.eval;
    }

    for (i = 0; i < moves.len; i++)
        score[i] = scoreMove(moves[i],depth,killer,ispv);

    for (i = 0; i < moves.len; i++){
        curInd = i;
        for (j = i+1; j < moves.len; j++)
            if (score[j] > score[curInd])
                curInd = j;
        swap(score[i],score[curInd]);
        moves.swap(i,curInd);
        m = moves[i];

        // if (rank == 5-(b.gameLen+1)/2 && rank == 3 && b.player == 1 && depth == 4)
        //     cout << showMove(m,cur,0);

        illegal = b.makeMove(m);
        if (illegal){
            b.unmakeMove();
            continue;
        }
        childpv.clear();
        cur = -negamax(depth-1, -beta, -alpha, killerNext, killerOpp, childpv, fullDepth-depth < pv.back().size() && ispv && m == pv.back()[fullDepth-depth]);
        b.unmakeMove();

        if (cur > beta){
            killer.second = killer.first;
            killer.first = m;
            if (b.sqs[square2(m)] == 0)
                butterfly[b.player][square1(m)][square2(m)] += depth^2;
            ttEntry.eval = best.eval; 
            ttEntry.m = best.m;
            ttEntry.type = CUT_NODE;
            tt.push(ttEntry);
            return cur;
        }

        if (cur > best.eval || gameOver){
            best.m = m;
            best.eval = cur;
            if (cur > alpha){
                alpha = cur;
                entry.type = PV_NODE;
            }
            if (depth > 0){
                parentpv.clear();
                parentpv.resize(childpv.size()+1);
                parentpv[0] = m;
                for (j = 0; j < childpv.size(); j++)
                    parentpv[j+1] = childpv[j];
            }
        }

        if (cur == MAX32)
            return MAX32;

        gameOver = false;

        if (over)
            return best.eval;
    }
    
    if (gameOver && depth > 0){
        if (b.attacked())
            return MIN32;
        return 0;
    }

    ttEntry.eval = best.eval; ttEntry.m = best.m;
    tt.push(ttEntry);
    return best.eval;
}

/// @brief loops through all moves and searches with negamax, returning best move; uses same search strategies as negamax
/// @param depth depth at which to search
/// @return best move for given depth
uint16_t engine::search(int32_t depth){
    nodes++;
    TTnode ttEntry = TTnode(b.zobrist,0,0,b.gameLen,depth);
    int32_t j , i, curInd, cur;
    uint16_t m;
    xMove best;
    vector<int32_t> score;
    score.resize(t.moves.len);
    pair<uint16_t,uint16_t> initKiller (0,0);
    vector<uint16_t> childpv;
    vector<uint16_t> nextpv;
    nextpv.resize(depth);
    bool illegal, gameOver = true;

    for (i = 0; i < t.moves.len; i++)
        score[i] = scoreMove(t.moves[i],depth,initKiller,false);

    for (i = 0; i < t.moves.len; i++){
        curInd = i;
        for (j = i+1; j < t.moves.len; j++)
            if (score[j] > score[curInd])
                curInd = j;
        m = t.moves[curInd];
        swap(score[i],score[curInd]);
        t.moves.swap(i,curInd);

        illegal = b.makeMove(m);
        if (illegal){
            b.unmakeMove();
            continue;
        }

        childpv.clear();
        cur = -negamax(depth-1, best.eval, MAX32, initKiller, initKiller, childpv, pv.back().size() > fullDepth-depth && m == pv.back()[fullDepth-depth]);
        b.unmakeMove();
        
        if (cur > best.eval || gameOver){
            best = xMove(cur, m);
            nextpv[0] = m;
            for (j = 0; j < childpv.size(); j++)
                nextpv[j+1] = childpv[j];
        }

        if (cur == MAX32)
            return m;

        gameOver = false;

        if (debug)
            cout << "info currmove " << algebraicFromShort(m) << " currmovenumber " << i+1 << " score cp " << cur << endl;

        if (over)
            return pv.back()[0];
    }
    
    ttEntry.eval = best.eval; ttEntry.m = best.m;
    tt.push(ttEntry);
    pv.push_back(nextpv);
    pveval.push_back(best.eval);
    return best.m;
}

/// @brief performs iterative deepening (ID), searching at increasing depths until time is exhausted
/// @param t_ contains the list of mvoes to consider, the cutoff mode (nodes/time/depth), and information about the time control
/// @return best move in position
uint16_t engine::getMove(task t_){
    selDepth = 0;
    forceStop = false;
    t = t_;
    if (t.mode == -1){
        t.mode = 0;
        t.length = initialTime();
    }
    if (t.moves.len == 0)
        t.moves = b.genMoves(false,false);
    nodes = 0;
    start = steady_clock::now();
    pv.clear();
    pveval.clear();
    pv.push_back(vector<uint16_t>(t.moves[0]));
    pveval.push_back(0);    
    uint16_t best;
    fullDepth = 1;
    over = false;   
    while (!over){
        best = search(fullDepth);
        auto passed = steady_clock::now()-start;
        cout << "info depth " << fullDepth << " seldepth " << fullDepth-selDepth << " nodes " << nodes << " time " << duration_cast<milliseconds>(passed).count() << " nps " << nodes/(duration_cast<seconds>(passed).count()+1) << " score cp " << pveval.back() << endl;
        cout << "info pv";
        for (int32_t i = 0; i < pv.back().size(); i++){
            if (pv.back()[i] == 0)
                break;
            cout << ' ' << algebraicFromShort(pv.back()[i]);
        }
        cout << endl;
        fullDepth++;
        if (pveval.back() == MAX32 || pveval.back() == MIN32)
            return best;
        over = checkOver();
    }
    return best;
}