:- use_module(library(random)).
:- use_module(library(lists)).
:- consult('utils.pl').
:- consult('io.pl').
:- consult('validMoves.pl').
:- consult('menu.pl').
:- consult('bots.pl').
:-dynamic score/2.
:-dynamic stones/2.

%initial(-Board)
%Initial board state
initial([
['R',' ',' ',' ',' ',' ','R'],
[' ',' ',' ',' ',' ',' ',' '],
[' ',' ',' ',' ',' ',' ',' '],
[' ',' ',' ',' ',' ',' ',' '],
[' ',' ',' ',' ',' ',' ',' '],
[' ',' ',' ',' ',' ',' ',' '],
['Y',' ',' ',' ',' ',' ','Y']
]).

%score(+Player, -score)
%Gets each player score
score('R', 0).
score('Y', 0).

%addScore(+Player,+Increment)
%Adds Increment points to player Player
addScore(Player,N) :- 
    retract(score(Player,Score)),
    AddedScore is Score + N, 
    assert(score(Player,AddedScore))
.

%stones(+Player, -NumStones)
%Gets each player stones left
stones('R',10).
stones('Y', 10).

%removeStone(+Player)
%Removes a 'O' from player count
removeStone(Player) :- 
    retract(stones(Player,Stones)), 
    ReducedStones is Stones -1, 
    assert(stones(Player,ReducedStones))
.

%dropStone(+Board , +Player, -NewBoard)
dropStone(Board , Player, NewBoard):-
    stones(Player,0),
    NewBoard = Board
.

dropStone(Board, Player, NewBoard):-
    readStonePosition(Board,Line,Column),
    replaceInMatrix(Board, Line, Column, 'O', NewBoard),
    removeStone(Player)
.

%addFishCount(+Board, +Line, +Column, +OldCount, -NewCount)
addFishCount(Board,Line,Column, OldCount, NewCount) :-
    at(Board, Line, Column, 'R'),
    NewCount is OldCount+1
.

addFishCount(Board,Line,Column, OldCount, NewCount) :-
    at(Board, Line, Column, 'Y'),
    NewCount is OldCount+1
.

addFishCount(_, _, _, OldCount, NewCount) :-
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

moveFish(Board,Player,FromLine,FromColumn, ToLine, ToColumn , NewBoard) :- 
    replaceInMatrix(Board,ToLine, ToColumn, Player, TempBoard),
    replaceInMatrix(TempBoard,FromLine,FromColumn,' ',NewBoard)
.

movePlayer(Board,Player,FromLine,FromColumn, ToLine, ToColumn , NewBoard) :-
    validWalk(Board, FromLine, FromColumn, ToLine, ToColumn),
    moveFish(Board,Player, FromLine, FromColumn, ToLine, ToColumn, TempBoard),
    dropStone(TempBoard, Player, NewBoard)
.

movePlayer(Board,Player,FromLine,FromColumn, ToLine, ToColumn , NewBoard) :-
    validJump(Board, FromLine, FromColumn, ToLine, ToColumn),
    moveFish(Board,Player, FromLine, FromColumn, ToLine, ToColumn, NewBoard)
.

movePlayer(Board, Player, FromLine , FromColumn, _ , _ , NewBoard) :-
    write('Invalid Move!\n'),
    readPlayerToPosition(ToLine, ToColumn),
    movePlayer(Board,Player,FromLine,FromColumn, ToLine, ToColumn , NewBoard)
.

moveByMode(Board, 'R', FromLine, FromColumn, ToLine, ToColumn, 'ComputerVsComputer') :-
    randomMove(Board, 'R', FromLine, FromColumn, ToLine, ToColumn)
.

moveByMode(Board, 'Y', FromLine, FromColumn, ToLine, ToColumn, 'ComputerVsComputer') :-
    bestMove(Board, 'Y', FromLine, FromColumn, ToLine, ToColumn)
.

moveByMode(Board, Computer, FromLine, FromColumn, ToLine, ToColumn, 'Easy') :-
    randomMove(Board, Computer, FromLine, FromColumn, ToLine, ToColumn)
.

moveByMode(Board, Computer, FromLine, FromColumn, ToLine, ToColumn, 'Hard') :-
    bestMove(Board, Computer, FromLine, FromColumn, ToLine, ToColumn)
.


nextTurn(Board, 'R', _) :-
    score('R', Score),
    Score > 9,
    cls,
    write('#########################################################'),nl,
    write('#                                                       #'),nl,
    write('#                 !!!CONGRATULATIONS!!!                 #'),nl,
    write('#                   !!RED PLAYER WON!!                  #'),nl,
    write('#                                                       #'),nl,
    write('#########################################################'),nl,
    displayBoard(Board),
    write('Press anything to continue...'),
    read_line(_),
    cls,
    mainMenu
.

nextTurn(Board, 'Y', _) :-
    score('Y', Score),
    Score > 9,
    cls,
    write('#########################################################'),nl,
    write('#                                                       #'),nl,
    write('#                 !!!CONGRATULATIONS!!!                 #'),nl,
    write('#                 !!YELLOW PLAYER WON!!                 #'),nl,
    write('#                                                       #'),nl,
    write('#########################################################'),nl,
    displayBoard(Board),
    write('Press anything to continue...'),
    read_line(_),
    cls,
    mainMenu 
.

nextTurn(Board, Player, 'Multiplayer') :-
    cls,
    nextPlayer(Player, NextPlayer),
    playerTurn(Board, NextPlayer, 'Multiplayer')
.

nextTurn(Board, Player, GameMode) :-
    nextPlayer(Player, Computer),
    
    moveByMode(Board, Computer, FromLine, FromColumn, ToLine, ToColumn, GameMode),
    
    validWalk(Board, FromLine, FromColumn, ToLine, ToColumn),
    stones(Computer, Stones),
    Stones > 0,

    moveFish(Board,Computer, FromLine, FromColumn, ToLine, ToColumn, TempBoard),
    findall([X,Y], at(TempBoard, X, Y, ' '), FreePlaces),
    random_member([StoneLine,StoneColumn], FreePlaces),
    replaceInMatrix(TempBoard, StoneLine, StoneColumn, 'O' , NewBoard),
    removeStone(Computer),
    
    nextTurnBot(Board, Player, Computer, NewBoard, ToLine, ToColumn, GameMode)
.

nextTurn(Board, Player, GameMode) :-
    nextPlayer(Player, Computer),
    
    moveByMode(Board, Computer, FromLine, FromColumn, ToLine, ToColumn, GameMode),
    moveFish(Board,Computer, FromLine, FromColumn, ToLine, ToColumn, NewBoard),
    
    nextTurnBot(Board, Player, Computer, NewBoard, ToLine, ToColumn, GameMode)
.

playerTurn(Board, 'R', 'ComputerVsComputer'):-
    nextTurn(Board, 'Y', 'ComputerVsComputer')
.

playerTurn(Board, 'Y', 'ComputerVsComputer'):-
    nextTurn(Board, 'R', 'ComputerVsComputer')
.

playerTurn(Board,Player,GameMode):-
    cls,
    displayGame(Board, Player) , 
    readPlayerFromPosition(Board, Player, FromLine, FromColumn),
    readPlayerToPosition(ToLine, ToColumn),
    movePlayer(Board, Player,FromLine, FromColumn, ToLine, ToColumn, NewBoard),
    countFish(NewBoard, ToLine, ToColumn, ScoreToAdd),
    addScore(Player,ScoreToAdd),
    nextTurn(NewBoard, Player, GameMode)
.

resetData :-
    retract(score('R',_)),
    retract(score('Y',_)),
    retract(stones('R',_)),
    retract(stones('Y',_)),

    assert(score('R',0)),
    assert(score('Y',0)),
    assert(stones('R',10)),
    assert(stones('Y',10))
.

%play/0
%Shows the initial state of the game
play :- cls, mainMenu.

mainMenu :-
    displayMainMenu,
    write('Select game mode: '),
    read_line(ChoiceTemp),
    nth0(0,ChoiceTemp,Choice),
    readChoice(Choice)
.

mainMenu('Invalid') :-
    cls,
    displayMainMenu,
    write('Invalid option!!! Select a game mode: '),
    read_line(ChoiceTemp),
    nth0(0,ChoiceTemp,Choice),
    readChoice(Choice)
.

%Single player easy
readChoice(49):-
    write('Choose a color (R,Y):'),
    read_line(Input),
    nth0(0,Input,ColorCode),
    selectColor(ColorCode),
    letter(ColorCode,Color),
    cls,
    resetData,
    initial(Board),
    playerTurn(Board, Color, 'Easy')
.

readChoice(49):-
    write('Invalid option!!! '),
    readChoice(49)
.

%Single player hard
readChoice(50):-
    write('Choose a color (R,Y):'),
    read_line(Input),
    nth0(0,Input,ColorCode),
    selectColor(ColorCode),
    letter(ColorCode,Color),
    cls,
    resetData,
    initial(Board),
    playerTurn(Board, Color, 'Hard')
.

readChoice(50):-
    write('Invalid option!!! '),
    readChoice(50)
.

%Multiplayer
readChoice(51):- 
    resetData,
    initial(Board), 
    random_member(Player,['R','Y']),
    playerTurn(Board,Player,'Multiplayer') 
.

%Computer vs computer
readChoice(52):-
    resetData,
    initial(Board),
    random_member(Starter,['R','Y']),
    playerTurn(Board, Starter, 'ComputerVsComputer')
.

%How to Play
readChoice(53):-
    cls, 
    howToPlay,
    read_line(_),
    cls,
    mainMenu
.

readChoice(54).

readChoice(_):- mainMenu('Invalid').

selectColor(82).
selectColor(89).