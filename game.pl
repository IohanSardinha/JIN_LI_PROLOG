:- use_module(library(random)).
:- use_module(library(lists)).
:- consult('utils.pl').
:- consult('display.pl').
%initial(-Board)
%Initial board state
initial([
[red,empty,empty,empty,empty,empty,red],
[empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty],
[yellow,empty,empty,empty,empty,empty,yellow]
]).

%middleBoard(-Board)
%Representation of a middle game board
middleBoard([
[empty,empty,empty,empty,empty,empty,empty],
[stone,empty,stone,empty,red,stone,empty],
[empty,empty,red,empty,stone,empty,stone],
[empty,stone,empty,empty,yellow,empty,empty],
[empty,empty,empty,stone,empty,empty,empty],
[empty,stone,yellow,empty,empty,empty,empty],
[empty,empty,empty,empty,stone,empty,empty]
]).

%finalBoard(-Board)
%Representation of a won game board
finalBoard([
[empty,empty,stone,empty,stone,empty,empty],
[stone,empty,stone,empty,empty,stone,empty],
[empty,stone,stone,empty,stone,empty,empty],
[empty,stone,empty,stone,stone,red,stone],
[empty,stone,empty,empty,empty,empty,red],
[stone,stone,yellow,stone,yellow,stone,empty],
[empty,stone,empty,empty,stone,stone,empty]
]).

%score(+Player, -score)
%Gets each player score
score(red, 0).
score(yellow, 0).

%addScore(+Player,+Increment)
%Adds Increment points to player Player
%addScore(Player,N) :- drop(Player,_), push(Player,N).

%stones(+Player, -NumStones)
%Gets each player stones left
stones(red,10).
stones(yellow, 10).

%removeStone(+Player)
%Removes a stone from player count
%removeStone(+Player) :- drop(+Player, N), N1 is N -1, push(+Player, N1).

%readPlayerFromPosition(+Player, -Line, -Column)
readPlayerFromPosition(Board, Player, Line, Column) :-
    write('Move from line(A-G):\n'), 
    read(LineLetter),
    letter(Line,LineLetter),
    write('Move from column(1-7):\n'),
    read(Column),
    at(Board, Line, Column, Player)
.

readPlayerFromPosition(Board, Player, Line, Column) :-
    write('Player not in given position!\n'),
    readPlayerFromPosition(Board,Player,Line,Column) 
.

%readPlayerToPosition(-Line, -Column)
readPlayerToPosition(Line,Column) :-
    write('Move to line(A-G):\n'), 
    read(LineLetter),
    letter(Line,LineLetter),
    
    write('Move to column(1-7):\n'),
    read(Column),
    
    Line < 8,
    Column < 8,
    Line > 0,
    Column > 0
.

readPlayerToPosition(Line,Column) :-
    write("Out of board!\n"),
    readPlayerToPosition(Line,Column)
.

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

%MovePlayer(+Board,+Player,-NewBoard)
movePlayer(Board,Player,FromLine,FromColumn, ToLine, ToColumn , NewBoard) :- 
    validMove(Board, FromLine, FromColumn, ToLine, ToColumn),
    ToLineIndex is ToLine - 1,
    ToColumnIndex is ToColumn -1,
    replaceInMatrix(Board,ToLineIndex, ToColumnIndex, Player, TempBoard),
    FromLineIndex is FromLine - 1,
    FromColumnIndex is FromColumn -1,
    replaceInMatrix(TempBoard,FromLineIndex,FromColumnIndex,empty,NewBoard)
.

movePlayer(Board, Player, FromLine , FromColumn, _ , _ , NewBoard) :-
    write('Invalid Move!\n'),
    readPlayerToPosition(ToLine, ToColumn),
    movePlayer(Board,Player,FromLine,FromColumn, ToLine, ToColumn , NewBoard)
.

playTurn(Board,Player):-
    displayGame(Board, Player) , 
    readPlayerFromPosition(Board, Player, FromLine, FromColumn),
    readPlayerToPosition(ToLine, ToColumn),
    movePlayer(Board, Player,FromLine, FromColumn, ToLine, ToColumn, NewBoard),
    displayGame(NewBoard,Player)
.

%play/0
%Shows the initial state of the game
play :-
    cls,
    initial(Board), 
    random_member(Player,[red,yellow]),
    playTurn(Board,Player)   
.