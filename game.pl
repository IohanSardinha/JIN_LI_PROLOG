:- use_module(library(random)).
:- use_module(library(lists)).
:- consult('utils.pl').
:- consult('display.pl').
:- consult('validMoves.pl').
:-dynamic score/2.
:-dynamic stones/2.
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

test([
[red,red,empty,empty,empty,empty,red],
[yellow,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty],
[yellow,empty,empty,empty,empty,empty,yellow]
]).

%score(+Player, -score)
%Gets each player score
score(red, 0).
score(yellow, 0).

%addScore(+Player,+Increment)
%Adds Increment points to player Player
addScore(Player,N) :- 
    retract(score(Player,Score)),
    AddedScore is Score + N, 
    assert(score(Player,AddedScore))
.

%stones(+Player, -NumStones)
%Gets each player stones left
stones(red,10).
stones(yellow, 10).

%removeStone(+Player)
%Removes a stone from player count
removeStone(Player) :- 
    retract(stones(Player,Stones)), 
    ReducedStones is Stones -1, 
    assert(stones(Player,ReducedStones))
.

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

readStonePosition(Board, Line, Column):-
    write("Dropped stone Line:\n"),
    read(LineLetter),
    letter(Line,LineLetter),

    write("Dropped stone column:\n"),
    read(Column),

    at(Board,Line,Column,empty)
.

readStonePosition(Board, Line, Column):-
    write("Unable to drop stone in that position!\n"),
    readStonePosition(Board,Line,Column)
.

dropStone(_, Player, _):-
    stones(Player,Stones),
    Stones < 1
.

dropStone(Board, Player, NewBoard):-
    readStonePosition(Board,Line,Column),
    LineIndex is Line -1,
    ColumnIndex is Column -1,
    replaceInMatrix(Board, LineIndex,ColumnIndex, stone, NewBoard),
    removeStone(Player)
.

%addFishCount(+Board, +Line, +Column, +OldCount, -NewCount)
addFishCount(Board,Line,Column, OldCount, NewCount) :-
    at(Board, Line, Column, red),
    NewCount is OldCount+1
.

addFishCount(Board,Line,Column, OldCount, NewCount) :-
    at(Board, Line, Column, yellow),
    NewCount is OldCount+1
.

addFishCount(_,_,_, OldCount, NewCount) :-
    NewCount = OldCount
.

countFish(Board, Line, Column, Count) :-
    Linep1 is Line + 1,
    Linem1 is Line -1,
    Columnp1 is Column + 1,
    Columnm1 is Column - 1, 
    addFishCount(Board,Linep1,Column, 0, Count2),
    addFishCount(Board,Linem1,Column, Count2, Count3),
    addFishCount(Board,Line,Columnp1, Count3, Count4),
    addFishCount(Board,Line,Columnm1, Count4, Count5),
    addFishCount(Board,Linep1,Columnp1, Count5, Count6),
    addFishCount(Board,Linem1,Columnp1, Count6, Count7),
    addFishCount(Board,Linep1,Columnm1, Count7, Count8),
    addFishCount(Board,Linem1,Columnm1, Count8, Count)
.

%MovePlayer(+Board,+Player,-NewBoard)
movePlayer(Board,Player,FromLine,FromColumn, ToLine, ToColumn , NewBoard) :- 
    validMove(Board, FromLine, FromColumn, ToLine, ToColumn),
    ToLineIndex is ToLine - 1,
    ToColumnIndex is ToColumn -1,
    replaceInMatrix(Board,ToLineIndex, ToColumnIndex, Player, TempBoard1),
    FromLineIndex is FromLine - 1,
    FromColumnIndex is FromColumn -1,
    replaceInMatrix(TempBoard1,FromLineIndex,FromColumnIndex,empty,TempBoard2),
    dropStone(TempBoard2, Player, NewBoard)
.

movePlayer(Board,Player,FromLine,FromColumn, ToLine, ToColumn , NewBoard) :- 
    validJump(Board, FromLine, FromColumn, ToLine, ToColumn),
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
    cls,
    displayGame(Board, Player) , 
    readPlayerFromPosition(Board, Player, FromLine, FromColumn),
    readPlayerToPosition(ToLine, ToColumn),
    movePlayer(Board, Player,FromLine, FromColumn, ToLine, ToColumn, NewBoard),
    countFish(NewBoard, ToLine, ToColumn, ScoreToAdd),
    addScore(Player,ScoreToAdd),
    nextPlayer(Player,NextPlayer),
    playTurn(NewBoard,NextPlayer)
.

%play/0
%Shows the initial state of the game
play :-
    initial(Board), 
    random_member(Player,[red,yellow]),
    playTurn(Board,Player)   
.