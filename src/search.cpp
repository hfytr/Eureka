#include <utility>
#include <vector>
#include <map>
#include <string>
#include <chrono>
#include <climits>
#include <iostream>
#include "board.h"
#include "constants.h"
#include "search.h"

void engine::printInfo(xMove best){
    if (debug == 2)
        return;
    auto passed = steady_clock::now()-start;
    std::cout <<
        "info depth " << fullDepth <<
        " nodes " << nodes <<
        " time " << duration_cast<milliseconds>(passed).count() <<
        " nps " << 1000 * nodes/(duration_cast<milliseconds>(passed).count()+1) <<
        " score cp " << best.eval << std::endl;

    std::cout << "info pv";
    for (uint8_t i = 0; i < lastpv.size(); i++){
        if (lastpv[i].isNull())
            break;
        std::cout << ' ' << Algebraic::move(lastpv[i]);
    }
    std::cout << std::endl;
}

uint32_t engine::initialTime(){
    uint32_t optimalTime;
    if (t.movestogo != MAX32)
        optimalTime = t.increment[b.getstm()] / 2 +
            t.timeLeft[b.getstm()] / t.movestogo;
    else
        optimalTime = t.increment[b.getstm()]/2 +
            t.timeLeft[b.getstm()] / 20;
    return std::min(optimalTime, t.timeLeft[b.getstm()]-moveOverhead);
}

/// @brief returns the result of a sequence of trades on a given square, or after a given move
/// @param sq the square on which trades will occurr
/// @param m the move which starts the sequence of captures
/// @return cp score DIFFERENCE after trades are finished
int32_t engine::see(Move m, uint8_t sq){
    int32_t attacker, victim = sq;
    Color stm = b.getstm();
    Bitboard traded = 0ULL;
    if (sq == 64){
        sq = m.square2();
        victim = sq;
        attacker = m.square1();
        traded.toggle(attacker);
    }
    traded.toggle(sq);

    if (m.isNull())
        attacker = b.lva(sq, traded, stm);
    if (b.getsq(sq).getType() == EMPTY)
        return 0;

    std::vector<int32_t> result, victimVal;
    while (attacker != 64){
        if (b.getsq(victim).getType() == KING)
            break;
        victimVal.push_back(b.val(victim));
        stm = opp(stm);
        victim = attacker;
        attacker = b.lva(sq,traded,stm);
        if (attacker != 64)
            traded.toggle(attacker);
    }

    result.resize(victimVal.size());
    result.push_back(0);
    for (int8_t i = victimVal.size()-1; i >= 0; i--)
        result[i] = std::max(0,victimVal[i] - result[i+1]);
    if (m.isNull())
        return result[0];
    return victimVal[0] - result[1]; // if m is provided we cannot decline the first capture
}

bool engine::checkOver(){
    if (forceStop)
        return true;
    switch (t.mode){
        case TIME:{
            auto passed = steady_clock::now()-start;
            return duration_cast<std::chrono::milliseconds>(passed).count() > t.length;
        }
        case NODES:
            return nodes > t.length;
        case DEPTH:
            return fullDepth > t.length;
    }
    return true;
}

/// @brief checks whether current board is line to debug
/// @return bool - true if is debug line else false
bool engine::isDbgLine(){
    std::vector<std::string> dbgLine = {"f6g4", "c4f7"};
    int32_t i;
    for (i = 0; i < dbgLine.size(); i++)
        if (i == b.getLen() || dbgLine[i] != Algebraic::move(b.getHist(i+1)))
            break;
    if (i == b.getLen())
        return true;
    return false;
}

scoredMoveList::scoredMoveList(uint8_t depth_, std::vector<Move> killers_, engine *e_, moveList list){
    depth= depth_;
    killers = std::move(killers_);
    e=e_;
    for (int32_t j = 0; j < list.len(); j++){
        see[length] = e->see(list[j]);
        if (see[length] >= SEE_CUTOFF * depth){
            container[length] = list[j];
            scores[length] = scoreMove();
            length++;
        }
    }
}

int32_t scoredMoveList::scoreMove(){
    if (e->tt.get(e->b.getZobrist()).m == container[length])
        return MAX32;

    bool capture = e->b.getsq(container[length].square2()).getType() != EMPTY;

    if (capture) {
        int32_t mvvlva = 1000 * e->b.val(container[length].square2(), true) + 1000 - e->b.val(container[length].square1(), true);
        if (see[length] >= 0)
            return mvvlva + GOOD_CAPTURE;
        return mvvlva + MIN32;
    }

    if (killers[0] == container[length] || killers[1] == container[length])
        return GOOD_CAPTURE-1;

    return std::min(e->butterfly[e->b.getstm()][container[length].square1()][container[length].square2()], GOOD_CAPTURE-2);
}

Move scoredMoveList::get(){
    int32_t curInd = i;
    for (int32_t j = i; j < length; j++)
        if (scores[j] > scores[curInd])
            curInd = j;
    std::swap(scores[i],scores[curInd]);
    swap(i,curInd);
    i++;
    return container[i-1];
}

int32_t engine::quiesce(int32_t alpha, int32_t beta){
    bool dbg = isDbgLine() && fullDepth == 1 && b.getLen() == 2;
    over = checkOver();
    nodes++;

    TTnode entry = tt.get(b.getZobrist());
    if ((entry.type == PV_NODE ||
        (entry.type == FAIL_HIGH && entry.m.eval >= beta) ||
        (entry.type == FAIL_LOW && entry.m.eval <= alpha)))
        return entry.m.eval;

    scoredMoveList moves = scoredMoveList(0, {NULLMOVE, NULLMOVE}, this, b.genMoves(false,true));

    xMove best;
    bool illegal;
    NodeType nodeType = FAIL_LOW;

    // it is (mostly) possible to not make any captures and instead "stand pat" so this is alpha's initial value
    best.eval = b.eval();
    if (alpha < best.eval)
        alpha = best.eval;
    if (best.eval >= beta)
        return best.eval;

    for (int32_t i = 0; i < moves.len(); i++){
        Move m = moves.get();
        illegal = b.makeMove(m);
        if (dbg)
            std::cout << m.print();
        if (illegal){
            b.unmakeMove();
            continue;
        }
        int32_t cur = -quiesce(-beta, -alpha);
        b.unmakeMove();

        if (cur >= beta){
            nodeType = FAIL_HIGH;
            tt.push(TTnode(b.getZobrist(), best, 0, nodeType));
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

    tt.push(TTnode(b.getZobrist(), best, 0, nodeType));
    return best.eval;
}

int32_t engine::negamax(uint8_t depth, int32_t alpha, int32_t beta){
    over = checkOver();
    nodes++;
    bool dbg = isDbgLine();

    TTnode entry = tt.get(b.getZobrist());
    if (isNullWindow(alpha, beta) &&
        !entry.m.isNull() &&
        entry.depth >= depth && (
            entry.type == PV_NODE ||
            (entry.type == FAIL_HIGH && entry.m.eval >= beta) ||
            (entry.type == FAIL_LOW && entry.m.eval <= alpha)))
        return entry.m.eval;

    xMove best;
    bool illegal, noMovesSearched = true, inCheck = b.attacked();
    NodeType nodeType = FAIL_LOW;
    uint8_t movesSearched = 0;

    int32_t curEval = b.eval();
    if (curEval >= beta){
        if (depth == 0)
            return curEval;
        int32_t nullEval = MIN32;
        if (!b.makeMove(NULLMOVE))
            nullEval = -negamax(depth-1, -beta, -beta+1);
        b.unmakeMove();
        if (nullEval >= beta)
            return nullEval;
    }
    
    if (depth == 0)
        return quiesce(alpha,beta);

    scoredMoveList moves = scoredMoveList(depth, killers[depth-1], this, b.genMoves(false,false));
    for (int32_t i = 0; i < moves.len(); i++){
        Move m = moves.get();

        illegal = b.makeMove(m);
        if (illegal){
            b.unmakeMove();
            continue;
        }
        else if (depth == 1 &&
            !b.attacked() &&
            b.getCapt(b.getLen()) == EMPTY &&
            curEval + FUTILITY_MARGIN < alpha) {
            b.unmakeMove();
            continue;
        }
        movesSearched++;
        int32_t cur;
        if (noMovesSearched)
            cur = -negamax(depth-1, -beta, -alpha);
        else {
            uint8_t reduction = movesSearched >= REDUCE_AFTER &&
                    b.getsq(m.square2()).getType() == EMPTY &&
                    depth >= 3 &&
                    !inCheck;
            cur =  -negamax(depth-1, -alpha, -alpha+1);
            if (cur > alpha && cur < beta)
                cur = -negamax(depth-1-reduction, -beta, -alpha);
        }
        b.unmakeMove();

        if (cur > best.eval || noMovesSearched){
            best = {cur, m};
            if (cur > alpha){
                nodeType = PV_NODE;
                alpha = cur;
                pv[depth-1][0] = m;
                for (int32_t j = 0; j < depth-1; j++) {
                    pv[depth-1][j+1] = pv[depth-2][j];
                    if (pv[depth-1][j+1].raw() == NULLMOVE)
                        break;
                }
            }
        }

        if (cur >= beta){
            if (b.getsq(m.square2()).getType() == EMPTY){
                killers[depth-1][1] = killers[depth-1][0];
                killers[depth-1][0] = m;
                butterfly[b.getstm()][m.square1()][m.square2()] += depth*depth;
            }
            nodeType = FAIL_HIGH;
            tt.push(TTnode(b.getZobrist(), best, depth, nodeType));
            if (depth > 2)
                killers[depth-3] = {0,0};
            return cur;
        }

        if (cur == CHECKMATE)
            return CHECKMATE;

        noMovesSearched = false;

        if (over)
            return best.eval;
    }
    
    if (noMovesSearched){
        if (b.attacked())
            return CHECKMATED;
        return 0;
    }

    tt.push(TTnode(b.getZobrist(), best, depth, nodeType));
    if (depth > 2)
        killers[fullDepth - depth + 2] = {NULLMOVE, NULLMOVE};
    return best.eval;
}
/*
position fen r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/2N2N2/PPPP1PPP/R1BQK2R b KQkq - 0 1
printboard
debug on
go depth 1

*/
/// @brief loops through all moves and searches with negamax, returning best move; uses same search strategies as negamax
/// @param depth depth at which to search
/// @return best move for given depth
xMove engine::search(uint8_t depth, int32_t alpha, int32_t beta){
    nodes++;
    xMove best = {MIN32,0};
    bool noMovesSearched = true, inCheck = b.attacked();
    uint8_t movesSearched = 0;
    
    if (b.eval() >= beta){
        int32_t nullEval = MIN32;
        if (!b.makeMove(NULLMOVE))
            nullEval = -negamax(depth-1, -beta, -beta+1);
        b.unmakeMove();
        if (nullEval >= beta)
            return nullEval;
    }

    scoredMoveList moves {depth, {NULLMOVE, NULLMOVE}, this, t.moves};

    for (int32_t i = 0; i < moves.len(); i++){
        Move m = moves.get();
        bool illegal = b.makeMove(m);

        if (illegal){
            b.unmakeMove();
            continue;
        }
        movesSearched++;
        int32_t cur;
        if (noMovesSearched)
            cur = -negamax(depth-1, -beta, -alpha);
        else{
            uint8_t reduction = movesSearched >= REDUCE_AFTER && b.getsq(m.square2()).getType() == EMPTY && depth >= 3 && !inCheck;
            cur =  -negamax(std::max(depth-1-reduction, 0), -alpha-1, -alpha);
            if (cur > alpha && cur < beta)
                cur = -negamax(depth-1, -beta, -alpha);
        }
        b.unmakeMove();

        if (cur > best.eval || noMovesSearched){
            best = xMove(cur, m);
            if (cur > alpha){
                alpha = cur;
                rootType = PV_NODE;
                pv[depth-1][0] = m;
                for (int32_t j = 0; j < depth-1; j++) {
                    pv[depth-1][j+1] = pv[depth-2][j];
                    if (pv[depth-1][j+1].raw() == NULLMOVE)
                        break;
                }
            }
        }

        if (cur >= beta){
            rootType = FAIL_HIGH;
            tt.push(TTnode(b.getZobrist(), {cur, m}, depth, rootType));
            return {cur,m};
        }

        if (cur == CHECKMATE)
            return {CHECKMATE, m};

        noMovesSearched = false;

        if (debug == 1)
            std::cout << m.print() << ' ' << cur << std::endl;

        if (over)
            return best;
    }
    
    tt.push(TTnode(b.getZobrist(), best, depth, rootType));
    if (rootType == PV_NODE)
        lastpv = pv[depth-1];
    return best;
}

//  r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/2N2N2/PPPP1PPP/R1BQK2R b KQkq - 0 1
/// @brief performs iterative deepening (ID), searching at increasing depths until time is exhausted
/// @param t_ contains the list of mvoes to consider, the cutoff mode (nodes/time/depth), and information about the time control
/// @return best move in position
xMove engine::getMove(task t_){
    forceStop = false;
    t = t_;
    if (t.mode == OPEN){
        t.mode = TIME;
        t.length = initialTime();
    }
    if (t.moves.len() == 0)
        t.moves = b.genMoves(false, false);

    nodes = 0;
    fullDepth = 1;
    start = steady_clock::now();
    pv.clear();
    pv.emplace_back(1, 0);
    over = false;
    int32_t alphaOffset = 40, betaOffset = 40;
    killers = {1,{NULLMOVE,NULLMOVE}};

    xMove best = search(1,MIN32,MAX32), cur;
    fullDepth = 2;
    pv.emplace_back(fullDepth, 0);
    
    while (!over){
        rootType = FAIL_LOW;
        killers = {fullDepth, {NULLMOVE, NULLMOVE}};
        cur = search(fullDepth, best.eval - alphaOffset, best.eval + betaOffset);
        switch(rootType){
            case PV_NODE:{
                printInfo(best);
                fullDepth++;
                pv.emplace_back(fullDepth,NULLMOVE);
                best = cur;
                over = checkOver();
                break;
            }
            case FAIL_LOW:{
                alphaOffset *= 2;
                break;
            }
            case FAIL_HIGH:{
                betaOffset *= 2;
                break;
            }
        }
        if (cur.eval == CHECKMATE || cur.eval == CHECKMATED)
            return best;

    }
    return best;
}