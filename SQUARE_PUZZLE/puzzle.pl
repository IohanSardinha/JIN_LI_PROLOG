:-use_module(library(clpfd)).
:- use_module(library(lists)).
:- consult('puzzles.pl').

%at(+Mat, +-Row, +-Col, -+Val)
at(Row, Col, Mat, Val) :- nth0(Row, Mat, ARow), nth0(Col, ARow, Val).
at(-1, _, _, 0).
at(_, -1, _, 0).

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
    build_column(N, Lines, Column),
    sum(Column, #=, Value)
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

square(Size, _ , _ , Size).

square(I, Size, Matrix, Size):-
    NewIndex is I + 1,
    square(NewIndex, 0, Matrix, Size)
.

square(I, J, Matrix, Size):-
    isUpperLeft(I, J, Matrix, IsUpperLeft),
    isSquare(I, J, Matrix, Size, IsSquare),

    IsUpperLeft #=> IsSquare,

    NewJ is J + 1,
    square(I, NewJ, Matrix, Size) 
.

isUpperLeft(I, J, Matrix, IsUpperLeft):-
    at(I, J, Matrix, Value),
    (Value #= 1) #<=> Filled,

    TopI is I -1,
    at(TopI, J, Matrix, TopValue),
    (TopValue #= 0) #<=> TopValid,

    LeftJ is J - 1,
    at(I, LeftJ, Matrix, LeftValue),
    (LeftValue #= 0) #<=> LeftValid,

    at(TopI, LeftJ, Matrix, TopLeftValue),
    (TopLeftValue #= 0) #<=> TopLeftValid,

    (Filled #/\ TopValid #/\ LeftValid #/\ TopLeftValid) #<=> IsUpperLeft
.

isSquare(I, J, Matrix, Size, IsSquare):-
    squareWidth(I, J, Size, Matrix, Width),
    squareHeight(I, J, Size, Matrix, Height),

    ((Height #>= 1) #/\ (Width #>= 1) #/\ (Height #= Width)) #<=> IsSquare
    
.

squareWidth(I, J, Size, Matrix, Length) :-
    NewJ is J + 1,
    squareWidth(I, NewJ, Size, Matrix, 1, Length, 1)       
.
squareWidth( _, Size, Size, _, _, Acc, Acc).
squareWidth(I,  J,   _ , Matrix, _, Acc, Acc) :-  at(I, J, Matrix, 0).
squareWidth(I,  J,   Size, Matrix, LastValue, Length, Acc) :- 
    at(I, J, Matrix, Value),
    ((Value #= 1) #/\  LastValue) #<=> Filled,

    NewAcc #= Acc + Filled,

    NewJ is J + 1,

    squareWidth(I, NewJ, Size, Matrix, Filled, Length, NewAcc)
.

squareHeight(I, J, Size, Matrix, Length) :-
    NewI is I + 1,
    squareHeight(NewI, J, Size, Matrix, 1, Length, 1)       
.
squareHeight( _, Size, Size, _, _, Acc, Acc).
squareHeight(I,  J,   _ , Matrix, _, Acc, Acc) :-  at(I, J, Matrix, 0).
squareHeight(I,  J,   Size, Matrix, LastValue, Length, Acc) :- 
    at(I, J, Matrix, Value),
    ((Value #= 1) #/\  LastValue) #<=> Filled,

    NewAcc #= Acc + Filled,

    NewI is I + 1,

    squareHeight(NewI, J, Size, Matrix, Filled, Length, NewAcc)
.

puzzle(LinesValues, ColumnsValues, Puzzle):-
    length(LinesValues, Dimension),
    lines(Dimension, LinesValues, Puzzle),
    columns(Dimension, ColumnsValues, Puzzle)
.

solve(LinesValues, ColumnsValues, Puzzle):-
    puzzle(LinesValues, ColumnsValues, Puzzle),
    flatten(Puzzle, FlattenedPuzzle),
    labeling([], FlattenedPuzzle),
    printMatrix(Puzzle)
.