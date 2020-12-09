:-use_module(library(clpfd)).
:- use_module(library(lists)).

printLine([]).
printLine([H|T]):- format(" ~w ",[H]), printLine(T).

printMatrix([]).
printMatrix([H|T]):- printLine(H), nl , printMatrix(T).

line(Size, Value, L):-
    length(L,Size),
    domain(L, 0, 1),
    sum(L, #=, Value)
.

lines( _ , [], []).
lines(Size, [H|T], [H1|T1]):-
    line(Size, H, H1),
    lines(Size, T, T1)
.

build_column(_, [], []).
build_column(N, [H|T], [V|T2]) :-
    nth1(N, H ,V),
    build_column(N, T, T2)
.

column(N, Value, Lines):-
    build_column(N, Lines, L),
    sum(L, #=, Value)
.

columns(0, [], _).
columns(N, [H|T], Lines):-
    length(Lines, Length),
    Position is Length - N + 1,
    column(Position, H, Lines),
    N1 is N -1,
    columns(N1, T, Lines)
.

flatten([],[]).
flatten([A,B], L):-
    append(A, B, L)
.
flatten([H|T], L):-
    flatten(T,L1),
    append(H, L1, L)
.

square(Dimension, LinesValues, ColumnsValues, Puzzle):-
    lines(Dimension, LinesValues, Puzzle),
    columns(Dimension, ColumnsValues, Puzzle),
    flatten(Puzzle, FlattenedPuzzle),
    labeling([], FlattenedPuzzle)
    %printMatrix(Puzzle)
.