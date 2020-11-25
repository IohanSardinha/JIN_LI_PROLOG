:- use_module(library(random)).
:- use_module(library(lists)).
:- consult('utils.pl').
:- consult('io.pl').
:- consult('validMoves.pl').
:- consult('menu.pl').
:- consult('bots.pl').
:-dynamic score/2.
:-dynamic stones/2.

%initial(-GameState)
%Initial board state
initial(
[
/*Board:*/ [
                ['R',' ',' ',' ',' ',' ','R'],
                [' ',' ',' ',' ',' ',' ',' '],
                [' ',' ',' ',' ',' ',' ',' '],
                [' ',' ',' ',' ',' ',' ',' '],
                [' ',' ',' ',' ',' ',' ',' '],
                [' ',' ',' ',' ',' ',' ',' '],
                ['Y',' ',' ',' ',' ',' ','Y']
            ],
/*Stones*/  [
      /*RED*/   10,
   /*YELLOW*/   10
            ],
/*Scores*/  [
      /*RED*/   0,
   /*YELLOW*/   0
            ]
]).

board(GameState, Board) :- nth0(0,GameState,Board).
updateBoard(GameState, NewBoard, NewGameState) :- replaceInList(GameState, 1, NewBoard, NewGameState).

%score(+Player, +GameState, -score)
%Gets each player score
score('R', GameState, Score):-nth0(2,GameState,Scores),nth0(0,Scores,Score).
score('Y', GameState, Score):-nth0(2,GameState,Scores),nth0(1,Scores,Score).

%addScore(+Player,+Increment)
%Adds Increment points to player Player
addScore(GameState, 'R', N, NewGameState) :- 
    score('R', GameState, Score),
    AddedScore is Score + N, 
    replaceInMatrix(GameState, 3, 1, AddedScore, NewGameState)
.

addScore(GameState, 'Y', N, NewGameState) :- 
    score('R', GameState, Score),
    AddedScore is Score + N, 
    replaceInMatrix(GameState, 3, 2, AddedScore, NewGameState)
.

%stones(+Player, -NumStones)
%Gets each player stones left
stones('R', GameState, Score):-nth0(1,GameState,Scores),nth0(0,Scores,Score).
stones('Y', GameState, Score):-nth0(1,GameState,Scores),nth0(1,Scores,Score).

%removeStone(+GameState, +Player, -NewGameState)
%Removes a stone from player count
removeStone(GameState, 'R', NewGameState) :- 
    stones('R', GameState, Stones), 
    ReducedStones is Stones -1, 
    replaceInMatrix(GameState, 2, 1, ReducedStones, NewGameState)
.
removeStone(GameState, 'Y', NewGameState) :- 
    stones('Y', GameState, Stones), 
    ReducedStones is Stones -1, 
    replaceInMatrix(GameState, 2, 2, ReducedStones, NewGameState)
.

%dropStone(+GameState , +Player, -NewBoard)
dropStone(GameState , Player, NewGameState):-
    stones(Player, GameState, 0),
    GameState = NewGameState
.

dropStone(GameState, Player, NewGameState):-
    board(GameState,Board),
    readStonePosition(Board,Line,Column),
    replaceInMatrix(Board, Line, Column, 'O', NewBoard),
    removeStone(GameState,Player, TempGameState),
    updateBoard(TempGameState, NewBoard, NewGameState)
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

moveFish(GameState,Player,FromLine,FromColumn, ToLine, ToColumn , NewGameState) :-
    board(GameState,Board), 
    replaceInMatrix(Board,ToLine, ToColumn, Player, TempBoard),
    replaceInMatrix(TempBoard,FromLine,FromColumn,' ',NewBoard),
    updateBoard(GameState,NewBoard, NewGameState)
.

movePlayer(GameState,Player,FromLine,FromColumn, ToLine, ToColumn , NewGameState) :-
    board(GameState,Board), 
    validWalk(Board, FromLine, FromColumn, ToLine, ToColumn),   
    moveFish(GameState,Player, FromLine, FromColumn, ToLine, ToColumn, TempGameState),
    dropStone(TempGameState, Player, NewGameState)
.

movePlayer(GameState,Player,FromLine,FromColumn, ToLine, ToColumn , NewGameState) :-
    board(GameState,Board), 
    validJump(Board, FromLine, FromColumn, ToLine, ToColumn),
    moveFish(GameState,Player, FromLine, FromColumn, ToLine, ToColumn, NewGameState)
.

movePlayer(GameState, Player, FromLine , FromColumn, _ , _ , NewGameState) :-
    write('Invalid Move!\n'),
    readPlayerToPosition(ToLine, ToColumn),
    movePlayer(GameState,Player,FromLine,FromColumn, ToLine, ToColumn , NewGameState)
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


nextTurn(GameState, 'R', _) :-
    board(GameState, Board),
    score('R',GameState, Score),
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

nextTurn(GameState, 'Y', _) :-
    board(GameState, Board),
    score('Y',GameState, Score),
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

nextTurn(GameState, Player, 'Multiplayer') :-
    cls,
    nextPlayer(Player, NextPlayer),
    playerTurn(GameState, NextPlayer, 'Multiplayer')
.

nextTurn(GameState, Player, GameMode) :-
    board(GameState, Board),
    nextPlayer(Player, Computer),
    
    moveByMode(Board, Computer, FromLine, FromColumn, ToLine, ToColumn, GameMode),
    
    validWalk(Board, FromLine, FromColumn, ToLine, ToColumn),
    stones(Computer,GameState, Stones),
    Stones > 0,

    moveFish(GameState,Computer, FromLine, FromColumn, ToLine, ToColumn, TempGameState),
    board(TempGameState, TempBoard),
    findall([X,Y], at(TempBoard, X, Y, ' '), FreePlaces),
    random_member([StoneLine,StoneColumn], FreePlaces),

    replaceInMatrix(TempBoard, StoneLine, StoneColumn, 'O' , NewBoard),
    removeStone(GameState,Computer,NewGameState),
    
    nextTurnBot(NewGameState, Player, Computer, NewBoard, ToLine, ToColumn, GameMode)
.

nextTurn(GameState, Player, GameMode) :-
    board(GameState, Board),
    nextPlayer(Player, Computer),
    
    moveByMode(Board, Computer, FromLine, FromColumn, ToLine, ToColumn, GameMode),
    moveFish(GameState,Computer, FromLine, FromColumn, ToLine, ToColumn, NewGameState),
    board(NewGameState, NewBoard),
    
    nextTurnBot(GameState, Player, Computer, NewBoard, ToLine, ToColumn, GameMode)
.

playerTurn(GameState, 'R', 'ComputerVsComputer'):-
    nextTurn(GameState, 'Y', 'ComputerVsComputer')
.

playerTurn(GameState, 'Y', 'ComputerVsComputer'):-
    nextTurn(GameState, 'R', 'ComputerVsComputer')
.

playerTurn(GameState,Player,GameMode):-
    cls,
    board(GameState, Board),
    displayGame(GameState, Player) , 
    readPlayerFromPosition(Board, Player, FromLine, FromColumn),
    readPlayerToPosition(ToLine, ToColumn),
    movePlayer(GameState, Player,FromLine, FromColumn, ToLine, ToColumn, TempGameState1),
    board(TempGameState1, NewBoard),
    countFish(NewBoard, ToLine, ToColumn, ScoreToAdd),
    addScore(TempGameState1,Player, ScoreToAdd,TempGameState2),
    updateBoard(TempGameState2, NewBoard, NewGameState),
    nextTurn(NewGameState, Player, GameMode)
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
    initial(GameState),
    playerTurn(GameState, Color, 'Easy')
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
    initial(GameState),
    playerTurn(GameState, Color, 'Hard')
.

readChoice(50):-
    write('Invalid option!!! '),
    readChoice(50)
.

%Multiplayer
readChoice(51):- 
    initial(GameState), 
    random_member(Player,['R','Y']),
    playerTurn(GameState,Player,'Multiplayer') 
.

%Computer vs computer
readChoice(52):-
    initial(GameState),
    random_member(Starter,['R','Y']),
    playerTurn(GameState, Starter, 'ComputerVsComputer')
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