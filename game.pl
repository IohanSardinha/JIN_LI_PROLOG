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

%readPlayerFromPosition(-Line, -Column)
readPlayerFromPosition(Line,Column) :-
    write('Move from line(A-G):\n'), 
    read(LineLetter),
    letter(Line,LineLetter),
    write('Move from column(1-7):\n'),
    read(Column)
.

%readPlayerToPosition(-Line, -Column)
readPlayerToPosition(Line,Column) :-
    write('Move to line(A-G):\n'), 
    read(LineLetter),
    letter(Line,LineLetter),
    write('Move to column(1-7):\n'),
    read(Column)
.

%MovePlayer(+Board,+Player,-NewBoard)
movePlayer(Board,Player,NewBoard) :- 
    readPlayerFromPosition(FromLine,FromColumn),
    at(Board,FromLine,FromColumn, Player),
    readPlayerToPosition(ToLine, ToColumn),
    replaceInMatrix(Board,ToLine-1, ToColumn-1, Player, TempBoard),
    replaceInMatrix(TempBoard,FromLine-1,FromColumn-1,empty,NewBoard)
.

playTurn(Board,Player):-
    displayGame(Board, Player) , 
    movePlayer(Board, Player, NewBoard),
    nextPlayer(Player,NextPlayer),
    playTurn(NewBoard, NextPlayer)
.

%play/0
%Shows the initial state of the game
play :- 
    initial(Board), 
    random_member(Player,[red,yellow]),
    playTurn(Board,Player)   
.