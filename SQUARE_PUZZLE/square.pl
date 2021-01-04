:- use_module(library(clpfd)).
:- use_module(library(lists)).

at(Row, Col, Mat, Val) :- nth0(Row, Mat, ARow), nth0(Col, ARow, Val).

puzzle(LinesValues, ColumnsValues) :-
    statistics(runtime, [T0|_]),

    length(LinesValues, LinesLength),
    size(LinesLength, Size),

    createRectangles(_, _, _, _, Rectangles, Lines, Columns, Lengths, Size, LinesLength),

    domain(Lines, 1, LinesLength),
    domain(Columns, 1, LinesLength),
    domain(Lengths, 0, LinesLength),

    disjoint2(Rectangles, [margin(a,a,1,1)]),
    lines(Lines, Lengths, 1, LinesValues),
    lines(Columns, Lengths, 1, ColumnsValues),

    sortSolution(Lines, Columns),

    append(Lines, Columns, V),
    append(V, Lengths, Vars), !,
    labeling([anti_first_fail, bisect, down], Vars), 

    statistics(runtime, [T1|_]),
    T is T1 - T0,nl,

    write(Lines), nl,
    write(Columns), nl,
    write(Lengths), nl,

    format('Time to solve: ~d milliseconds~n', [T])
    %convert(Lines, Columns, Lengths, LinesLength, Matrix), 

    %displayMatrix(Matrix, LinesLength, 1, LinesValues),nl,
    %displayColumns(0,LinesLength,ColumnsValues),!,nl
.
buildMatrix(Size, Matrix):-
    buildMatrix(Size, Size, Matrix)
.

buildMatrix(0, _ , []).

buildMatrix(Size, Length, [Line|Matrix]):-
    length(Line, Length),
    domain(Line, 0, 0),
    NS is Size - 1,
    buildMatrix(NS,Length, Matrix)
.

convert(Lines, Columns, Lengths, Size, Matrix):-
    buildMatrix(Size, Matrix),
    convert(0, Matrix, Lines, Columns, Lengths, Size)
.

convert(Index, Matrix, Lines, Columns, Lengths, Size):-
    nth0(Index, Lines, Line),
    nth0(Index, Columns, Column),
    nth0(Index, Length, Length)
.


sortSolution([_], [_]).
sortSolution([X1,X2|X], [Y1,Y2|Y]):-
    (X1 #= X2 #/\ Y1 #< Y2) #\/ X1 #< X2,
    sortSolution([X2|X],[Y2|Y]),!.


createRectangles(_, _, _, _, NewRectangles, NewLines, NewColumns, NewLengths, 1, FixedSize) :-
    NewRectangles = [rect(Ax, L1, Ay, L1, a)], 
    NewLines = [Ax], 
    NewColumns = [Ay], 
    NewLengths = [L1], 
    Ax + L1 #=< (FixedSize+1),
    Ay + L1 #=< (FixedSize+1)
.

createRectangles(Rectangles, Lines, Columns, Lengths, ResultRectangles, ResultStartX, ResultStartY, ResultLengths, Size, FixedSize) :-
    NewSize is Size - 1,
    createRectangles(Rectangles, Lines, Columns, Lengths, NewRectangles, NewLines, NewColumns, NewLengths, NewSize, FixedSize),
    append(NewRectangles, [rect(Ax, L1, Ay, L1, a)], ResultRectangles),
    append(NewLines, [Ax], ResultStartX),
    append(NewColumns, [Ay], ResultStartY),
    append(NewLengths, [L1], ResultLengths),
    Ax + L1 #=< (FixedSize+1),
    Ay + L1 #=< (FixedSize+1).


size(LinesLength, Size) :-
    Reminder is LinesLength mod 2,
    Reminder == 0,
    Size is (LinesLength // 2) * (LinesLength // 2)
.

size(LinesLength, Size) :-
    Size is ((LinesLength + 1)//2) * ((LinesLength + 1)//2)
.

lines(_, _, _, []).
lines(Coordenates, Lengths, LineNo, [LineTotal|RestTotals]):-
    check_line(Coordenates, Lengths, LineNo, Counter),
    LineNo2 is LineNo + 1,
    Counter #= LineTotal,
    lines(Coordenates, Lengths, LineNo2, RestTotals)
.

check_line([], [], _, 0).
check_line([X|RestX], [L|RestL], LineNo, Counter):-
    LineNo #>= X #/\  LineNo #< (X + L) #<=> B,
    Counter #= Counter2 + (B*L),
    check_line(RestX,RestL, LineNo, Counter2)
.
