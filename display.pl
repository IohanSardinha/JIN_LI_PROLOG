/*
initialBoard([
[red,empty,empty,empty,empty,empty,red],
[empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty],
[yellow,empty,empty,empty,empty,empty,yellow]
]).
middleBoard([
[empty,empty,empty,empty,empty,empty,empty],
[stone,empty,stone,empty,red,stone,empty],
[empty,empty,red,empty,stone,empty,empty],
[empty,red,empty,empty,yellow,empty,empty],
[empty,empty,empty,empty,empty,empty,empty],
[empty,stone,yellow,empty,empty,empty,empty],
[empty,empty,empty,empty,stone,empty,empty]
]).

finalBoard([
[empty,empty,empty,empty,stone,empty,empty],
[stone,empty,stone,empty,empty,stone,empty],
[empty,stone,stone,empty,stone,empty,empty],
[empty,stone,empty,empty,stone,red,empty],
[empty,empty,empty,empty,empty,empty,red],
[empty,stone,yellow,stone,yellow,stone,empty],
[empty,stone,empty,empty,stone,stone,empty]
]).

symbol(empty,S) :- S=' '.
symbol(yellow,S) :- S='Y'.
symbol(stone,S) :- S='O'.
symbol(red,S) :- S='R'.

letter(1, L) :- L='A'.
letter(2, L) :- L='B'.
letter(3, L) :- L='C'.
letter(4, L) :- L='D'.
letter(5, L) :- L='E'.
letter(6, L) :- L='F'.
letter(7, L) :- L='G'.


printBoard(X) :-
    nl,
    write('   | 1 | 2 | 3 | 4 | 5 | 6 | 7 |\n'),
    write('---|---|---|---|---|---|---|---|\n'),
    printLines(X, 1).

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


printObjects([]).
printObjects([Head|Tail]) :-
    symbol(Head, S),
    write(S),
    write(' | '),
    printObjects(Tail).

*/