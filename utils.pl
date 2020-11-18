replaceInList([_H|T], 0, Value, [Value|T]).
replaceInList([H|T], Index, Value, [H|TNew]) :-
        Index > 0,
        Index1 is Index - 1,
        replaceInList(T, Index1, Value, TNew).

replaceInMatrix([H|T], 0, Column,Value, [HNew|T]) :-
        replaceInList(H, Column, Value, HNew).

replaceInMatrix([H|T], Row, Column, Value, [H|TNew]) :-
        Row > 0,
        Row1 is Row - 1,
        replaceInMatrix(T, Row1, Column, Value, TNew).

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

letter(1, L) :- L=a.
letter(2, L) :- L=b.
letter(3, L) :- L=c.
letter(4, L) :- L=d.
letter(5, L) :- L=e.
letter(6, L) :- L=f.
letter(7, L) :- L=g.

%turn(+Player, -Color)
%Color of each player for display
turn(red,'Red').
turn(yellow,'Yellow').

%nextPlayer(+CurrentPlayer, NextPlayer)
nextPlayer(red,yellow).
nextPlayer(yellow,red).

%Clears the screen
cls :- cls(100).
cls(N) :- N > 0, write('\n'), N1 is N -1, cls(N1).
cls(_).

%at(+Mat, +-Row, +-Col, -+Val)
at(Mat, Row, Col, Val) :- nth1(Row, Mat, ARow), nth1(Col, ARow, Val).