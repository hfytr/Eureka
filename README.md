<h3 align="center">
	Eureka, a C++ Alpha-Beta Chess Engine
</h3>
<a href="https://lichess.org/@/EurekaEngine">
<img
        alt="Lichess"
        src="https://img.shields.io/badge/Play%20on%20LICHESS-v1.0.0-blue?logo=lichess&style=for-the-badge"
>
</a>

## Features
* **Evaluation:**
  * Material Development Evaluation
    * Material Evaluation
    * Piece Development Evaluation
  * Tapered Evaluation
    * Early Game
    * Late Game
* **Move Policy (Move Ordering)**:
  * Transposition Table Move (Normal Move Generation)
  * Promotion Move
    * Promotion Type
  * Capture Move
    * Most Valuable Victim with Least Valuable Attacker (MVVLVA)
    * Good/Bad captures via SEE
* **Search:**
  * Cancellation Support
  * Iterative Deepening
    * Depth Data Output
    * Principle Variation Output
    * Triangular PV Table
  * Aspiration Search
    * Narrow Windows
    * Bound-specific Widening
    * Fallback Threshold
  * Alpha-Beta Negamax
    * Three-fold Repetition Pruning
    * Transposition Table
      * Exact Cutoff
      * Alpha Unchanged Update
      * Beta Cutoff Update
      * Alpha Beta Delta Cutoff
    * Null Move Pruning
    * Fail-soft Alpha Beta Pruning
      * Futility Pruning
        * Evaluation Dependent
        * Alpha Dependent
      * Late Move Pruning
      * Late Move Reduction
        * Depth and Moves Played Logarithmic Reduction
        * At least three-ply
      * Principle Variation Search
        * Full Search on First Move
  * Quiescence Search (QSearch)
    * Static Evaluation
      * Beta Cutoff
      * Alpha Update
    * Fail-soft Alpha Beta Pruning
      * Static Exchange Evaluation (SEE) Pruning
