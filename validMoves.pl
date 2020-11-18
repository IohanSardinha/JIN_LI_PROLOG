% [ ][ ][ ]     [ ][ ][ ]
% [ ][X][ ] --> [ ][ ][X]
% [ ][ ][ ]     [ ][ ][ ]
validMove(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine + 1,
    ToColumn =:= FromColumn,
    at(Board, ToLine, ToColumn, empty)
.
% [ ][ ][ ]     [ ][ ][X]
% [ ][X][ ] --> [ ][ ][ ]
% [ ][ ][ ]     [ ][ ][ ]
validMove(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine + 1,
    ToColumn =:= FromColumn + 1,
    at(Board, ToLine, ToColumn, empty)
.
% [ ][ ][ ]     [ ][ ][ ]
% [ ][X][ ] --> [ ][ ][ ]
% [ ][ ][ ]     [ ][ ][X]
validMove(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine + 1,
    ToColumn =:= FromColumn - 1,
    at(Board, ToLine, ToColumn, empty)
.
% [ ][ ][ ]     [ ][ ][ ]
% [ ][X][ ] --> [X][ ][ ]
% [ ][ ][ ]     [ ][ ][ ]
validMove(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine - 1,
    ToColumn =:= FromColumn,
    at(Board, ToLine, ToColumn, empty)
.
% [ ][ ][ ]     [X][ ][ ]
% [ ][X][ ] --> [ ][ ][ ]
% [ ][ ][ ]     [ ][ ][ ]
validMove(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine - 1,
    ToColumn =:= FromColumn + 1,
    at(Board, ToLine, ToColumn, empty)
.
% [ ][ ][ ]     [ ][ ][ ]
% [ ][X][ ] --> [ ][ ][ ]
% [ ][ ][ ]     [X][ ][ ]
validMove(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine - 1,
    ToColumn =:= FromColumn - 1,
    at(Board, ToLine, ToColumn, empty)
.
% [ ][ ][ ]     [ ][ ][ ]
% [ ][X][ ] --> [ ][ ][ ]
% [ ][ ][ ]     [ ][X][ ]
validMove(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine,
    ToColumn =:= FromColumn - 1,
    at(Board, ToLine, ToColumn, empty)
.
% [ ][ ][ ]     [ ][X][ ]
% [ ][X][ ] --> [ ][ ][ ]
% [ ][ ][ ]     [ ][ ][ ]
validMove(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine,
    ToColumn =:= FromColumn + 1,
    at(Board, ToLine, ToColumn, empty)
.

% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [ ][S][ ][S][X]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine,
    ToColumn =:= FromColumn+2,
    StoneColumn is FromColumn+1,
    at(Board, ToLine, ToColumn, empty),
    at(Board, FromLine, StoneColumn, stone)
.

% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [X][S][ ][S][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine,
    ToColumn =:= FromColumn-2,
    StoneColumn is FromColumn-1,
    at(Board, ToLine, ToColumn, empty),
    at(Board, FromLine, StoneColumn, stone)
.

% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [X][S][ ][S][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine,
    ToColumn =:= FromColumn-2,
    StoneColumn is FromColumn-1,
    at(Board, ToLine, ToColumn, empty),
    at(Board, FromLine, StoneColumn, stone)
.

% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][X]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [ ][S][ ][S][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine+2,
    ToColumn =:= FromColumn+2,
    StoneLine is FromLine + 1,
    StoneColumn is FromColumn+1,
    at(Board, ToLine, ToColumn, empty),
    at(Board, StoneLine, StoneColumn, stone)
.

% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [ ][S][ ][S][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][X]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine-2,
    ToColumn =:= FromColumn+2,
    StoneLine is FromLine - 1,
    StoneColumn is FromColumn+1,
    at(Board, ToLine, ToColumn, empty),
    at(Board, StoneLine, StoneColumn, stone)
.

% [ ][ ][ ][ ][ ]     [X][ ][ ][ ][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [ ][S][ ][S][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine+2,
    ToColumn =:= FromColumn-2,
    StoneLine is FromLine + 1,
    StoneColumn is FromColumn-1,
    at(Board, ToLine, ToColumn, empty),
    at(Board, StoneLine, StoneColumn, stone)
.

% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [ ][S][ ][S][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [X][ ][ ][ ][ ]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine-2,
    ToColumn =:= FromColumn-2,
    StoneLine is FromLine - 1,
    StoneColumn is FromColumn-1,
    at(Board, ToLine, ToColumn, empty),
    at(Board, StoneLine, StoneColumn, stone)
.

% [ ][ ][ ][ ][ ]     [ ][ ][X][ ][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [ ][S][ ][S][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine+2,
    ToColumn =:= FromColumn,
    StoneLine is FromLine + 1,
    at(Board, ToLine, ToColumn, empty),
    at(Board, StoneLine, FromColumn, stone)
.

% [ ][ ][ ][ ][ ]     [ ][ ][ ][ ][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][S][X][S][ ] --> [ ][S][ ][S][ ]
% [ ][S][S][S][ ]     [ ][S][S][S][ ]
% [ ][ ][ ][ ][ ]     [ ][ ][X][ ][ ]
validJump(Board, FromLine, FromColumn, ToLine, ToColumn) :-
    ToLine =:= FromLine-2,
    ToColumn =:= FromColumn,
    StoneLine is FromLine - 1,
    at(Board, ToLine, ToColumn, empty),
    at(Board, StoneLine, FromColumn, stone)
.