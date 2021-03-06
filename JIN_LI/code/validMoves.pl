% [ ][ ][ ]     [ ][ ][ ]
% [ ][X][ ] --> [ ][ ][X]
% [ ][ ][ ]     [ ][ ][ ]
validWalk(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine + 1,
    ToColumn is FromColumn,
    at(Board, ToLine, ToColumn, ' ')
.
% [ ][ ][ ]     [ ][ ][X]
% [ ][X][ ] --> [ ][ ][ ]
% [ ][ ][ ]     [ ][ ][ ]
validWalk(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine + 1,
    ToColumn is FromColumn + 1,
    at(Board, ToLine, ToColumn, ' ')
.
% [ ][ ][ ]     [ ][ ][ ]
% [ ][X][ ] --> [ ][ ][ ]
% [ ][ ][ ]     [ ][ ][X]
validWalk(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine + 1,
    ToColumn is FromColumn - 1,
    at(Board, ToLine, ToColumn, ' ')
.
% [ ][ ][ ]     [ ][ ][ ]
% [ ][X][ ] --> [X][ ][ ]
% [ ][ ][ ]     [ ][ ][ ]
validWalk(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine - 1,
    ToColumn is FromColumn,
    at(Board, ToLine, ToColumn, ' ')
.
% [ ][ ][ ]     [X][ ][ ]
% [ ][X][ ] --> [ ][ ][ ]
% [ ][ ][ ]     [ ][ ][ ]
validWalk(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine - 1,
    ToColumn is FromColumn + 1,
    at(Board, ToLine, ToColumn, ' ')
.
% [ ][ ][ ]     [ ][ ][ ]
% [ ][X][ ] --> [ ][ ][ ]
% [ ][ ][ ]     [X][ ][ ]
validWalk(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine - 1,
    ToColumn is FromColumn - 1,
    at(Board, ToLine, ToColumn, ' ')
.
% [ ][ ][ ]     [ ][ ][ ]
% [ ][X][ ] --> [ ][ ][ ]
% [ ][ ][ ]     [ ][X][ ]
validWalk(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine,
    ToColumn is FromColumn - 1,
    at(Board, ToLine, ToColumn, ' ')
.
% [ ][ ][ ]     [ ][X][ ]
% [ ][X][ ] --> [ ][ ][ ]
% [ ][ ][ ]     [ ][ ][ ]
validWalk(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine,
    ToColumn is FromColumn + 1,
    at(Board, ToLine, ToColumn, ' ')
.

% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [ ][S][ ][S][X]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine,
    ToColumn is FromColumn+2,
    StoneColumn is FromColumn+1,
    at(Board, ToLine, ToColumn, ' '),
    at(Board, FromLine, StoneColumn, 'O')
.

% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [X][S][ ][S][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine,
    ToColumn is FromColumn-2,
    StoneColumn is FromColumn-1,
    at(Board, ToLine, ToColumn, ' '),
    at(Board, FromLine, StoneColumn, 'O')
.

% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [X][S][ ][S][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine,
    ToColumn is FromColumn-2,
    StoneColumn is FromColumn-1,
    at(Board, ToLine, ToColumn, ' '),
    at(Board, FromLine, StoneColumn, 'O')
.

% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][X]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [ ][S][ ][S][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine+2,
    ToColumn is FromColumn+2,
    StoneLine is FromLine + 1,
    StoneColumn is FromColumn+1,
    at(Board, ToLine, ToColumn, ' '),
    at(Board, StoneLine, StoneColumn, 'O')
.

% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [ ][S][ ][S][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][X]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine-2,
    ToColumn is FromColumn+2,
    StoneLine is FromLine - 1,
    StoneColumn is FromColumn+1,
    at(Board, ToLine, ToColumn, ' '),
    at(Board, StoneLine, StoneColumn, 'O')
.

% [ ][ ][ ][ ][ ]     [X][ ][ ][ ][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [ ][S][ ][S][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine+2,
    ToColumn is FromColumn-2,
    StoneLine is FromLine + 1,
    StoneColumn is FromColumn-1,
    at(Board, ToLine, ToColumn, ' '),
    at(Board, StoneLine, StoneColumn, 'O')
.

% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [ ][S][ ][S][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [X][ ][ ][ ][ ]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine-2,
    ToColumn is FromColumn-2,
    StoneLine is FromLine - 1,
    StoneColumn is FromColumn-1,
    at(Board, ToLine, ToColumn, ' '),
    at(Board, StoneLine, StoneColumn, 'O')
.

% [ ][ ][ ][ ][ ]     [ ][ ][X][ ][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [ ][S][ ][S][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine+2,
    ToColumn is FromColumn,
    StoneLine is FromLine + 1,
    at(Board, ToLine, ToColumn, ' '),
    at(Board, StoneLine, FromColumn, 'O')
.

% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [ ][S][ ][S][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [ ][ ][X][ ][ ]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine is FromLine-2,
    ToColumn is FromColumn,
    StoneLine is FromLine - 1,
    at(Board, ToLine, ToColumn, ' '),
    at(Board, StoneLine, FromColumn, 'O')
.

valid_moves(GameState, Player, ListOfMoves):-
    board(GameState, Board),
    findall([X,Y], at(Board, X, Y, Player), [[FirstX,FirstY],[SecondX,SecondY]]),
    findAllPossibleMoves(Board, FirstX, FirstY, AllFirstPossibleMoves),
    findAllPossibleMoves(Board, SecondX, SecondY, AllSecondPossibleMoves),
    append(AllFirstPossibleMoves,AllSecondPossibleMoves, ListOfMoves)
.