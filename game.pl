:- use_module(library(random)).
:- use_module(library(lists)).
:- consult('utils.pl').
:- consult('io.pl').
:- consult('validMoves.pl').
:- consult('menu.pl').
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

test([
[' ',' ',' ',' ',' ',' ',' '],
[' ',' ',' ',' ',' ',' ',' '],
[' ',' ','R',' ','R',' ',' '],
[' ',' ',' ',' ',' ',' ',' '],
[' ',' ',' ','Y',' ',' ',' '],
[' ',' ',' ',' ',' ',' ',' '],
[' ',' ',' ',' ',' ','Y',' ']
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

distanceSum( _ , _ , [], 0, _).
distanceSum(FromX, FromY, [[ToX,ToY]], Sum, Counter):-
    distance2D(FromX, FromY, ToX, ToY, Distance),
    Sum is Counter + Distance
.
distanceSum(FromX, FromY, [[ToX,ToY]|Tail], Sum, Temp):-
    distance2D(FromX, FromY, ToX, ToY, Distance),
    Counter is Temp + Distance,
    distanceSum(FromX, FromY, Tail, Sum, Counter)
.

distanceSum(FromX, FromY, Positions, Sum):-distanceSum(FromX, FromY, Positions, Sum, 0).

%findAllDistances(PossibleMoves, OtherFishes, Distances, Accumulator).
findAllDistances([], _ ,[],_).

findAllDistances([[ToLine,ToColumn]], OtherFishes, Distances, Accumulator):-
    distanceSum(ToLine, ToColumn, OtherFishes, Sum),
    append(Accumulator, [Sum], Distances)
.

findAllDistances([[ToLine,ToColumn]|Tail], OtherFishes, Distances, Temp):-
    distanceSum(ToLine, ToColumn, OtherFishes, Sum),
    append(Temp,[Sum], Accumulator),
    findAllDistances(Tail, OtherFishes, Distances, Accumulator)
.
findAllDistances([[ToLine,ToColumn]|Tail], OtherFishes, Distances):-findAllDistances([[ToLine,ToColumn]|Tail], OtherFishes, Distances, []).

findAllOtherFishes(Board, FishLine, FishColumn, OtherFishes):-
    findall([X,Y], (at(Board, X, Y, 'R'), [X,Y] \= [FishLine,FishColumn]), OtherFishesRed),
    findall([X,Y], (at(Board, X, Y, 'Y'), [X,Y] \= [FishLine,FishColumn]), OtherFishesYellow),
    append(OtherFishesRed,OtherFishesYellow,OtherFishes)
.

findAllPossibleMoves(Board, Line, Column, AllPossibleMoves):-
    findall([X,Y],(validWalk(Board,Line,Column,X,Y)),AllPossibleWalks),
    findall([X2,Y2],(validJump(Board,Line,Column,X2,Y2)),AllPossibleJumps),
    append(AllPossibleWalks,AllPossibleJumps,AllPossibleMoves)
.

%findAllMovesScores(Board, FromX, FromY, AllPossibleMoves, Scores, Accumulator)
findAllMovesScores(_, _, _ , [] , _, [] ).
findAllMovesScores(Board, FromX, FromY, [[ToX,ToY]], Scores, Accumulator):-
    at(Board, FromX, FromY, Value),
    replaceInMatrix(Board, FromX, FromY, ' ', TempBoard),
    replaceInMatrix(TempBoard, ToX, ToY, Value, NewBoard),
    countFish(NewBoard, ToX, ToY, Score),
    append(Accumulator,[Score], Scores)
.
findAllMovesScores(Board, FromX, FromY, [[ToX,ToY]|Tail], Scores, Temp):-
    at(Board, FromX, FromY, Value),
    replaceInMatrix(Board, FromX, FromY, ' ', TempBoard),
    replaceInMatrix(TempBoard, ToX, ToY, Value, NewBoard),
    countFish(NewBoard, ToX, ToY, Score),
    append(Temp,[Score], Accumulator),
    findAllMovesScores(Board, FromX, FromY, Tail, Scores, Accumulator)
.
findAllMovesScores(Board, FromX, FromY, [[ToX,ToY]|Tail], Scores):- findAllMovesScores(Board, FromX, FromY, [[ToX,ToY]|Tail], Scores, []).



%bestMove(+Board, +X, +Y, -BestMove, -BestDistance)
bestMove(Board, X, Y, BestMove, BestDistance):-
    findAllOtherFishes(Board, X, Y, OtherFishes),
    findAllPossibleMoves(Board, X, Y, AllPossibleMoves),
    findAllMovesScores(Board, X, Y, AllPossibleMoves, Scores),
    findAllDistances(AllPossibleMoves, OtherFishes, Distances),
    listSub(Distances, Scores, DistancesLessScores),
    listSum(Distances, Scores, DistancesPlusScores),
    min_member(SmallestDistance,DistancesLessScores),
    nth0(Index, DistancesLessScores, SmallestDistance),
    nth0(Index, DistancesPlusScores, BestDistance),
    nth0(Index, AllPossibleMoves, BestMove),!
.

bestMove(Board, Player, FromX, FromY, ToX, ToY):-
    findall([X,Y], at(Board, X, Y, Player), [[FromX,FromY],[SecondX,SecondY]]),
    
    bestMove(Board, FromX, FromY, [ToX, ToY], FirstMinDistance),
    bestMove(Board, SecondX, SecondY, [_,_], SecondMinDistance),

    format('Best 1: ~w, best 2: ~w~n', [FirstMinDistance,SecondMinDistance]),
    format('~w~n',[[FromX,FromY, SecondX, SecondY]]),
    FirstMinDistance > SecondMinDistance,
    write('Move First\n')
    
.

bestMove(Board, Player, FromX, FromY, ToX, ToY):-
    findall([X,Y], at(Board, X, Y, Player), [[_,_],[FromX,FromY]]),
    bestMove(Board, FromX, FromY, [ToX, ToY], SecondMinDistance ),
    write('Move Second\n')
.

smartBotTurn(Board,Player, FromX, FromY, ToX, ToY):-
    bestMove(Board, Player, FromX, FromY, ToX, ToY),
    movePlayer(Board, Player, FromX, FromY, ToX, ToY, NewBoard),
    displayBoard(NewBoard),
    read_line(_),
    smartBotTurn(NewBoard,Player, _, _, _, _)
.

dumbBotTurn(Board, Player, FromLine, FromColumn, ToLine, ToColumn):-
    findall([X,Y], at(Board, X, Y, Player), PlayerFishes),
    random_member(PlayerFish, PlayerFishes),
    [FromLine,FromColumn] = PlayerFish,
    findAllPossibleMoves(Board, FromLine, FromColumn, AllPossibleMoves),
    random_member(Move, AllPossibleMoves),
    [ToLine, ToColumn] = Move
.

%MovePlayer(+Board,+Player,-NewBoard)
movePlayer(Board,Player,FromLine,FromColumn, ToLine, ToColumn , NewBoard) :- 
    validWalk(Board, FromLine, FromColumn, ToLine, ToColumn),
    replaceInMatrix(Board,ToLine, ToColumn, Player, TempBoard1),
    replaceInMatrix(TempBoard1,FromLine,FromColumn,' ',NewBoard)
    %dropStone(TempBoard2, Player, NewBoard)
.

movePlayer(Board,Player,FromLine,FromColumn, ToLine, ToColumn , NewBoard) :- 
    validJump(Board, FromLine, FromColumn, ToLine, ToColumn),
    replaceInMatrix(Board,ToLine, ToColumn, Player, TempBoard),
    replaceInMatrix(TempBoard,FromLine,FromColumn,' ',NewBoard)
.

movePlayer(Board, Player, FromLine , FromColumn, _ , _ , NewBoard) :-
    write('Invalid Move!\n'),
    readPlayerToPosition(ToLine, ToColumn),
    movePlayer(Board,Player,FromLine,FromColumn, ToLine, ToColumn , NewBoard)
.

nextTurn(Board, 'R') :-
    score('R', Score),
    Score > 9,
    cls,
    write('Gongratulations!!!\nPlayer RED won the game!!!\n'),
    displayBoard(Board)
.

nextTurn(Board, 'Y') :-
    score('Y', Score),
    Score > 9,
    cls,
    write('Gongratulations!!!\nPlayer YELLOW won the game!!!\n'),
    displayBoard(Board) 
.

nextTurn(Board, Player) :-
    cls,
    nextPlayer(Player, NextPlayer),
    playTurn(Board, NextPlayer)
.

playTurn(Board,Player):-
    cls,
    displayGame(Board, Player) , 
    readPlayerFromPosition(Board, Player, FromLine, FromColumn),
    readPlayerToPosition(ToLine, ToColumn),
    movePlayer(Board, Player,FromLine, FromColumn, ToLine, ToColumn, NewBoard),
    countFish(NewBoard, ToLine, ToColumn, ScoreToAdd),
    addScore(Player,ScoreToAdd),
    nextTurn(NewBoard, Player)
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
play :-
    mainMenu,
    read_line(ChoiceTemp),
    nth0(0,ChoiceTemp,Choice),
    readChoice(Choice)
       
.


readChoice(49):- 
    resetData,
    initial(Board), 
    random_member(Player,['R','Y']),
    playTurn(Board,Player) 
.