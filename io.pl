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

%readPlayerFromPosition(+Player, -Line, -Column)
readPlayerFromPosition(Board, Player, Line, Column) :-
    write('Move from line(A-G):\n'), 
    read_line(LineLetter),
    nth0(0,LineLetter,Linetemp),
    letter(Line,Linetemp),
    write('Move from column(1-7):\n'),
    read_line(ColumnCode),
    nth0(0,ColumnCode,Columntemp),
    letter(Column,Columntemp),
    at(Board, Line, Column, Player)
.

readPlayerFromPosition(Board, Player, Line, Column) :-
    write('Player not in given position!\n'),
    readPlayerFromPosition(Board,Player,Line,Column) 
.

%readPlayerToPosition(-Line, -Column)
readPlayerToPosition(Line,Column) :-
    write('Move to line(A-G):\n'), 
    read_line(LineLetter),
    nth0(0,LineLetter,Linetemp),
    letter(Line,Linetemp),
    write('Move to column(1-7):\n'),
    read_line(ColumnCode),
    nth0(0,ColumnCode,Columntemp),
    letter(Column,Columntemp),
    
    Line < 8,
    Column < 8,
    Line > 0,
    Column > 0
.

readPlayerToPosition(Line,Column) :-
    write('Out of board!\n'),
    readPlayerToPosition(Line,Column)
.

readStonePosition(Board, Line, Column):-
    write('Dropped stone Line:\n'),
    read_line(LineLetter),
    nth0(0,LineLetter,Linetemp),
    letter(Line,Linetemp),

    write('Dropped stone column:\n'),
    read_line(ColumnCode),
    nth0(0,ColumnCode,Columntemp),
    letter(Column,Columntemp),

    at(Board,Line,Column,' ')
.

readStonePosition(Board, Line, Column):-
    write('Unable to drop stone in that position!\n'),
    readStonePosition(Board,Line,Column)
.