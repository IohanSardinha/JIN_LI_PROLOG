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