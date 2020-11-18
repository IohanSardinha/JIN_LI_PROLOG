%displayGame(+GameState,+Player)
%Display the given state of the game and the current turn player
displayGame(GameState,Player) :-
    displayHeader(Player),
    displayBoard(GameState)
.

displayHeader(Player):-
    score('R',RedScore),
    score('Y',YellowScore),
    stones(Player, Stones),
    displayTurn(Player),
    format("Stones: ~w~n",Stones),
    format("Score: ~n", []),
    format("    Red: ~w     Yellow: ~w~n",[RedScore,YellowScore])
.

displayTurn('Y') :- format("Turn: YELLOW~n",[]).
displayTurn('R') :- format("Turn: RED~n",[]).

displayBoard(GameState):-
    write('   | 1 | 2 | 3 | 4 | 5 | 6 | 7 |\n'),
    write('---|---|---|---|---|---|---|---|\n'),
    printLines(GameState, 1)
.

printLines([],8).
printLines([FirstLine|BoardLeftover], N):-
    letter(N,LineLetter),
    format(' ~w | ~w | ~w | ~w | ~w | ~w | ~w | ~w |\n',[LineLetter|FirstLine]),
    write('---|---|---|---|---|---|---|---|\n'),
    N1 is N + 1,
    printLines(BoardLeftover, N1)   
.