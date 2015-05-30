fen 
===

fen - Rewrite of FEN utility in haskell


Feed it PGN-notation chess moves and it produces the corresponding chess position, FEN-encoded.
Also checks move legality.

intended usage:

    > fen "e4 c5"
    rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1
    rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2

Note: Work in progress


TODOs
- Finish test suite + utils in Test.hs
- Write PGN move parsing functionality
- Write main.hs (parse command line args, display errors in a user-friendly way etc)
- Support move translation, e.g "e4 -> KP4"
- Support loading entire .pgn-files and parsing out all the moves and resulting positions


Long term TODOs
- Develop a GUI (websockets??)
- Allow importing, searching etc to chess game database
- Support talking to chess engine
