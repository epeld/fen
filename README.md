fen 
===

fen - Rewrite of FEN utility in haskell


Feed it PGN-notation chess moves and it produces the corresponding chess position, FEN-encoded.
Also checks move legality.

intended usage:

    > fen "e4 c5"
    rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1
    rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2

Later on, we can add a fancy UI with e.g FRP javascript and websockets 'n stuff.

# How to Run
Currently, nothing is working really, except the test suites. To generate them do
```bash
    scons
```
then
```bash
    cabal test
```
if you are lucky the `scons`-command should have generated a `test-suites`-directory for you,
and `cabal test` ran the tests.

# TODOs
- Write PGN move parsing functionality (define ParserT types, parse out PartialMoves, test)
- Write main.hs (parse command line args, display errors in a user-friendly way etc)
- Support move translation, e.g "e4 -> KP4" (would be cool!)
- Support loading entire .pgn-files and parsing out all the moves and resulting positions


## Long term TODOs
- Develop a GUI (websockets??)
- Allow importing, searching etc to chess game database
- Support talking to chess engine
