AI checker player written in Haskell for an academic project.
It uses alpha-beta prunning + MTD(f) + iterative deepening + quiescence search to calculate and explore search tree.
I wasn't able to beat this program :) But I'm not the best checker player.

https://en.wikipedia.org/wiki/Alpha–beta_pruning
https://en.wikipedia.org/wiki/MTD-f
https://en.wikipedia.org/wiki/Iterative_deepening_depth-first_search
https://www.chessprogramming.org/Quiescence_Search

Internal board representation: bitboard

Compile:
ghc -O2 main.hs

Run:
main.exe [w|b]

w - play as white
b - play as black

You can specify max time for ai in: ai.hs
Default: 2 minutes

Checker rules: https://en.wikipedia.org/wiki/English_draughts
