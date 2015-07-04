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

# Architectural Overview
The logic is centered around the concept of 'promoting' moves.

You start with something like a `PartialMove`, where 
```
'Partial' is intended to mean 'lacking information that uniquely identifies the move'.
```
You can then use the `FullMove` or `LegalMove` to promote your moves. That is, by supplying more information.

The `LegalMove`-module can take any type of (partially specified) move and calculate all the legal moves that said move *could* represent.

Note that, currently castling moves cannot be processed (Work in Progress).

In the end, when the program is run it will be the responsibility of the user to always supply enough information to uniquely determine a move.
If this requirement is not satisfied, there will be an error: "Ambiguous move. Candidates are: [List of candidates]"

Modules of importance:
- All Move-modules: `Move`, `PartialMove`, `FullMove`, `LegalMove`
- `UpdatedPosition` produces a new position, given a legal move
- To parse moves out of strings, see `PgnParse`.

# TODOs
- Define data type that supports both normal moves and castling
- Write PGN move parsing functionality (test)
- Write main.hs (parse command line args, display errors in a user-friendly way etc)
- Support move translation, e.g "e4 -> KP4" (would be cool!)
- Support loading entire .pgn-files and parsing out all the moves and resulting positions


## Long term TODOs
- Develop a GUI (elm, websockets??)
- Allow importing, searching etc to chess game database
- Support talking to chess engine
