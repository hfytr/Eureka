#include <vector>
#include <map>
#include <string>
#include <chrono>
#include <climits>
#include <iostream>
#include "board.h"
#include "constants.h"
#include "search.h"

void engine::printInfo(){
    auto passed = steady_clock::now()-start;
    if (!debug && duration_cast<std::chrono::milliseconds>(passed).count() < std::min(500,t.length/2)) // reduce verbosity
        return;
    std::cout << "info depth " << fullDepth << " nodes " << nodes << " time " << duration_cast<milliseconds>(passed).count() << " nps " << nodes/(duration_cast<seconds>(passed).count()+1) << " score cp " << pveval.back() << std::endl;
    std::cout << "info pv";
    for (int32_t i = 0; i < pv.back().size(); i++){
        if (pv.back()[i] == 0)
            break;
        std::cout << ' ' << algebraicFromShort(pv.back()[i]);
    }
    std::cout << std::endl;
}

int32_t engine::initialTime(){
    int32_t optimalTime;
    if (t.movestogo > 0)
        optimalTime = t.increment[b.player]/2 + t.timeLeft[b.player]/t.movestogo;
    else
        optimalTime = t.increment[b.player]/2 + t.timeLeft[b.player]/20;
    return std::min(optimalTime, t.timeLeft[b.player]-20);
}

/// @brief returns the result of a sequence of trades on a given square, or after a given move
/// @param sq the square on which trades will occurr
/// @param m the move which starts the sequence of captures
/// @return cp score DIFFERENCE after trades are finished
int32_t engine::see(uint16_t m, int32_t sq){
    int32_t attacker, victim = sq, player = b.player;
    uint64_t traded = 1ULL << sq;
    if (sq == -1){
        sq = square2(m);
        victim = sq;
        attacker = square1(m);
    }
    if (m == 0)
        attacker = b.lva(sq, traded, player);
    std::vector<int32_t> result, victimVal;
    while (attacker != -1){
        victimVal.push_back(b.val(victim));
        player = opp(player);
        victim = attacker;
        if (pType(b.sqs[victim]) == 6){
            victimVal.pop_back();
            break;
        }
        attacker = b.lva(sq,traded,player);
        if (attacker != -1)
            traded ^= 1ULL << attacker;
    }
    result.resize(victimVal.size());
    result.push_back(0);
    for (int32_t i = victimVal.size()-1; i >= 0; i--)
        result[i] = std::max(0,victimVal[i] - result[i+1]);
    return result[0];
}

bool engine::checkOver(){
    if (forceStop)
        return true;
    switch (t.mode){
        case 0:{    
            auto passed = steady_clock::now()-start;
            return duration_cast<std::chrono::milliseconds>(passed).count() > t.length;
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
    std::vector<std::string> dbgLine = {"b4d2","f2g2"};
    int32_t i;
    for (i = 0; i < dbgLine.size(); i++)
        if (i == b.gameLen || dbgLine[i] != algebraicFromShort(b.gameHist[i+1]))
            break;
    if (i == b.gameLen && fullDepth == 3)
        return true;
    return false;
}

scoredMoveList::scoredMoveList(uint8_t depth_, bool ispv_, std::pair<uint16_t, uint16_t> killers_, engine *e_, moveList list){
    depth=depth_;ispv=ispv_;killers=killers_;e=e_;
    for (int32_t j = 0; j < list.len(); j++){
        container[j] = list[j];
        scores[j] = scoreMove(container[j]);
        length++;
    }
}

int32_t scoredMoveList::scoreMove(uint16_t m){
    if (ispv && e->fullDepth-depth < e->pv.back().size() && m == e->pv.back()[e->fullDepth-depth])
        return MAX32;

    if (e->tt.get(e->b.zobrist).m == m)
        return MAX32-1;

    bool capture = e->b.sqs[square2(m)] != 0;

    if (capture)
        return 1000*e->b.val(square2(m), true) + 1000 - e->b.val(square1(m), true) + MINMVVLVA;

    if (killers.first == m || killers.second == m)
        return MINMVVLVA-1;

    return std::min(e->butterfly[e->b.player][square1(m)][square2(m)],MINMVVLVA-2);
}

uint16_t scoredMoveList::get(){
    int32_t curInd = i;
    for (int32_t j = i+1; j < length; j++)
        if (scores[j] > scores[curInd])
            curInd = j;
    std::swap(scores[i],scores[curInd]);
    swap(i,curInd);
    i++;
    return container[i-1];
}

int32_t engine::quiesce(int32_t alpha, int32_t beta){
    over = checkOver();
    nodes++;

    TTnode entry = tt.get(b.zobrist);
    if (entry.m != 0 && (entry.type == PV_NODE || (entry.type == FAIL_HIGH && entry.eval >= beta) || (entry.type == FAIL_LOW && entry.eval <= alpha)))
        return entry.eval;

    scoredMoveList moves = scoredMoveList(0, false, {0,0}, this, b.genMoves(false,true));

    xMove best;
    bool illegal;
    uint8_t nodeType = FAIL_LOW;

    // it is (mostly) possible to not make any captures and instead "stand pat" so this is alpha's initial value
    best.eval = b.eval();
    if (alpha < best.eval)
        alpha = best.eval;
    if (best.eval >= beta)
        return best.eval;

    for (int32_t i = 0; i < moves.len(); i++){
        uint16_t m = moves.get();
        illegal = b.makeMove(m);
        if (illegal){
            b.unmakeMove();
            continue;
        }
        int32_t cur = -quiesce(-beta, -alpha);
        b.unmakeMove();

        if (cur >= beta){
            nodeType = FAIL_HIGH;
            tt.push(TTnode(b.zobrist, best.eval, best.m, 0, nodeType));
            return cur;
        }

        if (cur > best.eval){
            best = {cur, m};
            if (cur > alpha){
                nodeType = PV_NODE;
                alpha = cur;
            }
        }

        if (over)
            return best.eval;
    }

    tt.push(TTnode(b.zobrist, best.eval, best.m, 0, nodeType));
    return best.eval;
}

int32_t engine::negamax(uint8_t depth, int32_t alpha, int32_t beta, std::vector<uint16_t> &parentpv, bool ispv){
    over = checkOver();
    nodes++;

    TTnode entry = tt.get(b.zobrist);
    if (entry.m != 0 && entry.depth >= depth && (entry.type == PV_NODE || (entry.type == FAIL_HIGH && entry.eval >= beta) || (entry.type == FAIL_LOW && entry.eval <= alpha)))
        return entry.eval;

    if (depth == 0)
        return quiesce(alpha,beta);

    scoredMoveList moves = scoredMoveList(0, ispv, {0,0}, this, b.genMoves(false,false));

    xMove best;
    std::vector<uint16_t> childpv;
    bool illegal, gameOver = true;
    uint8_t nodeType = FAIL_LOW;

    for (int32_t i = 0; i < moves.len(); i++){
        uint16_t m = moves.get();

        illegal = b.makeMove(m);
        if (illegal){
            b.unmakeMove();
            continue;
        }
        childpv.clear();
        bool searchingpv = fullDepth-depth < pv.back().size() && ispv && m == pv.back()[fullDepth-depth];
        int32_t cur = -negamax(depth-1, -beta, -alpha, childpv, searchingpv);
        b.unmakeMove();

        if (cur >= beta){
            if (b.sqs[square2(m)] == 0){
                killers[fullDepth-depth].second = killers[fullDepth-depth].first;
                killers[fullDepth-depth].first = m;
                butterfly[b.player][square1(m)][square2(m)] += depth*depth;
            }
            nodeType = FAIL_HIGH;
            tt.push(TTnode(b.zobrist, best.eval, best.m, depth, nodeType));
            if (depth > 2)
                killers[fullDepth - depth + 2] = std::pair<uint16_t,uint16_t> (0,0);
            return cur;
        }

        if (cur > best.eval || gameOver){
            best = {cur, m};
            if (cur > alpha){
                nodeType = PV_NODE;
                alpha = cur;
                parentpv.clear();
                parentpv.resize(childpv.size()+1);
                parentpv[0] = m;
                for (int32_t j = 0; j < childpv.size(); j++)
                    parentpv[j+1] = childpv[j];
            }
        }

        if (cur == CHECKMATE)
            return CHECKMATE;

        gameOver = false;

        if (over)
            return best.eval;
    }
    
    if (gameOver){
        if (b.attacked())
            return CHECKMATED;
        return 0;
    }

    tt.push(TTnode(b.zobrist, best.eval, best.m, depth, nodeType));
    if (depth > 2)
        killers[fullDepth - depth + 2] = std::pair<uint16_t,uint16_t> (0,0);
    return best.eval;
}

/// @brief loops through all moves and searches with negamax, returning best move; uses same search strategies as negamax
/// @param depth depth at which to search
/// @return best move for given depth
xMove engine::search(uint8_t depth, int32_t alpha, int32_t beta){
    killers.resize(depth);
    killers.resize(depth);
    nodes++;
    xMove best = {MIN32,0};
    std::vector<uint16_t> childpv;
    std::vector<uint16_t> nextpv;
    nextpv.resize(depth);
    bool gameOver = true;

    scoredMoveList moves {depth, true, {}, this, t.moves};

    for (int32_t i = 0; i < moves.len(); i++){
        uint16_t m = moves.get();
        bool illegal = b.makeMove(m);

        if (illegal){
            b.unmakeMove();
            continue;
        }

        childpv.clear();
        int32_t cur = -negamax(depth-1, MIN32, -best.eval, childpv, pv.back().size() > fullDepth-depth && m == pv.back()[fullDepth-depth]);
        b.unmakeMove();

        if (cur > best.eval || gameOver){
            best = {cur, m};
            if (cur > alpha){
                alpha = cur;
                rootType = PV_NODE;
                nextpv[0] = m;
                for (int32_t j = 0; j < childpv.size(); j++)
                    nextpv[j+1] = childpv[j];
            }
        }

        if (cur >= beta){
            rootType = FAIL_HIGH;
            tt.push(TTnode(b.zobrist, cur, m, depth, FAIL_HIGH));
            return {cur,m};
        }

        if (cur == CHECKMATE)
            return m;

        gameOver = false;

        if (debug)
            std::cout << "info currmove " << algebraicFromShort(m) << " currmovenumber " << i+1 << " score cp " << cur << std::endl << std::endl;

        if (over)
            return {pveval.back(), pv.back()[0]};
    }
    
    tt.push(TTnode(b.zobrist, best.eval, best.m, depth, rootType));
    if (rootType == PV_NODE){
        pv.push_back(nextpv);
        pveval.push_back(best.eval);
    }
    return best;
}

/// @brief performs iterative deepening (ID), searching at increasing depths until time is exhausted
/// @param t_ contains the list of mvoes to consider, the cutoff mode (nodes/time/depth), and information about the time control
/// @return best move in position
uint16_t engine::getMove(task t_){
    forceStop = false;
    t = t_;
    if (t.mode == -1){
        t.mode = 0;
        t.length = initialTime();
    }
    if (t.moves.len() == 0)
        t.moves = b.genMoves(false, false);

    nodes = 0;
    start = steady_clock::now();
    pv.clear();
    pveval.clear();
    pv.emplace_back(t.moves[0]);
    pveval.push_back(0);
    over = false;
    int32_t alphaOffset = 40, betaOffset = 40;

    xMove best = search(1,MIN32,MAX32), cur;
    fullDepth = 2;
    
    while (!over){
        rootType = FAIL_LOW;
        cur = search(fullDepth, best.eval - alphaOffset, best.eval + betaOffset);
        switch(rootType){
            case PV_NODE:{ 
                fullDepth++;
                printInfo();
                if (pveval.back() == CHECKMATE || pveval.back() == CHECKMATED)
                    return best.m;
                over = checkOver();
                break;
            }
            case FAIL_LOW:{
                alphaOffset *= 2;
                best = cur;
                break;
            }
            case FAIL_HIGH:{
                betaOffset *= 2;
                best = cur;
                break;
            }
        }
    }
    return best.m;
}