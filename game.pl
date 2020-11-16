:- consult('utils.pl').
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

%symbol(+state,-symbol).
%Symbol to be printed from each board cell state
symbol(empty,S) :- S=' '.
symbol(yellow,S) :- S='Y'.
symbol(stone,S) :- S='O'.
symbol(red,S) :- S='R'.

%letter(+Number, -Letter).
%Letter to be printed for each board row
letter(1, L) :- L='A'.
letter(2, L) :- L='B'.
letter(3, L) :- L='C'.
letter(4, L) :- L='D'.
letter(5, L) :- L='E'.
letter(6, L) :- L='F'.
letter(7, L) :- L='G'.

%turn(+Player, -Color)
%Color of each player for display
turn(red,'Red').
turn(yellow,'Yellow').

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

%displayGame(+GameState,+Player)
%Display the given state of the game and the current turn player
displayGame(GameState,Player) :-
    turn(Player, P),
    score(red,RedScore),
    score(yellow,YellowScore),
    stones(Player,Stones),
    format("Turn: ~w~n",P),
    format("Stones: ~w~n",Stones),
    format("Score: ~n",[]),
    format("    Red: ~w     Yellow: ~w~n",[RedScore,YellowScore]),
    write('   | 1 | 2 | 3 | 4 | 5 | 6 | 7 |\n'),
    write('---|---|---|---|---|---|---|---|\n'),
    printLines(GameState, 1).

%printLines(+GameState, +Line)
%Prints the Nth line of the Board
printLines([], 8).
printLines([Head|Tail], N) :-
    letter(N, L),
    write(' '),
    write(L),
    N1 is N + 1,
    write(' | '),
    printObjects(Head),
    write('\n---|---|---|---|---|---|---|---|\n'),
    printLines(Tail, N1).

%printObjects(+Line)
%Prints the symbols in a line of the board
printObjects([]).
printObjects([Head|Tail]) :-
    symbol(Head, S),
    write(S),
    write(' | '),
    printObjects(Tail).

%play()
%Shows the initial state of the game
play:- initial(Board),middleBoard(Board2),turn(FirstPlayer,'Yellow'), displayGame(Board,FirstPlayer),replaceInMatrix(Board,5,1,yellow,NewBoard),replaceInMatrix(NewBoard,6,0,empty,NewNewBoard),displayGame(NewNewBoard,FirstPlayer).
