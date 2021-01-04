:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- include('display.pl').

puzzle(LinesValues, ColumnsValues) :-
    statistics(runtime, [T0|_]),

    length(LinesValues, LinesLength),
    size(LinesLength, Size),

    createRectangles(_, _, _, _, Rectangles, StartX, StartY, Lengths, Size, LinesLength),

    domain(StartX, 1, LinesLength),
    domain(StartY, 1, LinesLength),
    domain(Lengths, 0, LinesLength),

    disjoint2(Rectangles, [margin(a,a,1,1)]),
    lines(StartX, Lengths, 1, LinesValues),
    lines(StartY, Lengths, 1, ColumnsValues),

    sortSolution(StartX, StartY),

    append(StartX, StartY, V),
    append(V, Lengths, Vars), !,
    labeling([anti_first_fail, bisect, down], Vars), 

    statistics(runtime, [T1|_]),
    T is T1 - T0,nl,
    format('Time to solve: ~d milliseconds.~n', [T]),

    %write(StartX), nl,
    %write(StartY), nl,
    %write(Lengths), nl,

    convert(StartX, StartY, Lengths, LinesLength, Matrix), 

    displayMatrix(Matrix, LinesLength, 1, LinesValues),nl,
    displayColumns(0,LinesLength,ColumnsValues),!,nl
.


sortSolution([_], [_]).
sortSolution([X1,X2|X], [Y1,Y2|Y]):-
    (X1 #= X2 #/\ Y1 #< Y2) #\/ X1 #< X2,
    sortSolution([X2|X],[Y2|Y]),!.


% ----------------------- Converts Lists to Matrix ---------------------------------
filter_lists_Aux(StartX, StartY, Lengths, 1, StartXFiltered, StartYFiltered, LengthsFiltered) :-
    nth1(1, Lengths, Elem),
    Elem > 0,
    append([], [Elem], LengthsFiltered),
    nth1(1, StartX, ElemX),
    nth1(1, StartY, ElemY),
    append([], [ElemX], StartXFiltered),
    append([], [ElemY], StartYFiltered)
.

filter_lists_Aux(StartX, StartY, Lengths, 1, StartXFiltered, StartYFiltered, LengthsFiltered) :-
    nth1(NewLengthsSize, Lengths, Elem),
    Elem == 0,
    StartXFiltered = [],
    StartYFiltered = [],
    LengthsFiltered = [].
filter_lists_Aux(StartX, StartY, Lengths, LengthsSize, NewStartXFiltered, NewStartYFiltered, NewLengthsFiltered) :-
    NewLengthsSize is LengthsSize - 1,
    nth1(LengthsSize, Lengths, Elem),
    Elem > 0,
    filter_lists_Aux(StartX, StartY, Lengths, NewLengthsSize, StartXFiltered, StartYFiltered, LengthsFiltered),
    append(LengthsFiltered, [Elem], NewLengthsFiltered),
    nth1(LengthsSize, StartX, ElemX),
    nth1(LengthsSize, StartY, ElemY),
    append(StartXFiltered, [ElemX], NewStartXFiltered),
    append(StartYFiltered, [ElemY], NewStartYFiltered).
filter_lists_Aux(StartX, StartY, Lengths, LengthsSize, NewStartXFiltered, NewStartYFiltered, NewLengthsFiltered) :-
    NewLengthsSize is LengthsSize - 1,
    nth1(LengthsSize, Lengths, Elem),
    Elem == 0,
    filter_lists_Aux(StartX, StartY, Lengths, NewLengthsSize, NewStartXFiltered, NewStartYFiltered, NewLengthsFiltered).

filter_lists(StartX, StartY, Lengths, StartXFiltered, StartYFiltered, LengthsFiltered) :-
    length(Lengths, LengthsSize),
    filter_lists_Aux(StartX, StartY, Lengths, LengthsSize, StartXFiltered, StartYFiltered, LengthsFiltered).


build_matrix(LinesLength, LinesLength, [H | []]) :-
    length(List, LinesLength),
    append([], List, H).
build_matrix(LinesLength, ColumnSize, [H | T]) :-
    NewColumnSize is ColumnSize + 1,
    build_matrix(LinesLength, NewColumnSize, T),
    length(List, LinesLength),
    append([], List, H).


fill_row(StartY, 1, Max, Row) :-
    nth1(StartY, Row, Elem),
    Elem = 1.
fill_row(StartY, Size, SizeConst, Row) :-
    NewStartY is StartY + 1,
    NewSize is Size - 1,
    fill_row(NewStartY, NewSize, SizeConst, Row),
    nth1(StartY, Row, Elem),
    Elem = 1.


fill_aux(StartX, StartY, 1, SizeConst, Matrix) :-
    nth1(StartX, Matrix, Row),
    Max is StartY + 1,
    fill_row(StartY, SizeConst, Max, Row).
fill_aux(StartX, StartY, Size, SizeConst, Matrix) :-
    NewStartX is StartX + 1,
    NewSize is Size -1,
    fill_aux(NewStartX, StartY, NewSize, SizeConst, Matrix),
    nth1(StartX, Matrix, Row),
    fill_row(StartY, SizeConst, SizeConst, Row).


fill_matrix(StartXFiltered, StartYFiltered, LengthsFiltered, 1, Matrix, FilledMatrix) :-
    nth1(1, StartXFiltered, StartXNumber),
    nth1(1, StartYFiltered, StartYNumber),
    nth1(StartXNumber, Matrix, Row),
    nth1(StartYNumber, Row, Elem),
    Elem = 1,
    nth1(1, LengthsFiltered, Size),
    fill_aux(StartXNumber, StartYNumber, Size, Size, Matrix).
fill_matrix(StartXFiltered, StartYFiltered, LengthsFiltered, AuxSize, Matrix, FilledMatrix) :-
    NewAuxSize is AuxSize - 1,
    fill_matrix(StartXFiltered, StartYFiltered, LengthsFiltered, NewAuxSize, Matrix, FilledMatrix),
    nth1(AuxSize, StartXFiltered, StartXNumber),
    nth1(AuxSize, StartYFiltered, StartYNumber),
    nth1(StartXNumber, Matrix, Row),
    nth1(StartYNumber, Row, Elem),
    Elem = 1,
    nth1(AuxSize, LengthsFiltered, Size),
    fill_aux(StartXNumber, StartYNumber, Size, Size, Matrix).


complete_aux(Row, 1) :-
    nth1(1, Row, Elem),
    Elem \== 1,
    Elem = 0.
complete_aux(Row, 1).
complete_aux(Row, LinesLength) :-
    nth1(LinesLength, Row, Elem),
    Elem \== 1,
    Elem = 0,
    NewRowSize is LinesLength - 1,
    complete_aux(Row, NewRowSize).
complete_aux(Row, LinesLength) :-
    nth1(LinesLength, Row, Elem),
    Elem == 1,
    NewRowSize is LinesLength - 1,
    complete_aux(Row, NewRowSize).

complete_matrix(Matrix, 1, LinesLength) :-
    nth1(1, Matrix, Row), 
    complete_aux(Row, LinesLength).
complete_matrix(Matrix, LinesLength, ConstRowSize) :-
    NewRowSize is LinesLength - 1, 
    complete_matrix(Matrix, NewRowSize, ConstRowSize),
    nth1(LinesLength, Matrix, Row), 
    complete_aux(Row, ConstRowSize).

convert(StartX, StartY, Lengths, LinesLength, Matrix) :-
    filter_lists(StartX, StartY, Lengths, StartXFiltered, StartYFiltered, LengthsFiltered),
    build_matrix(LinesLength, 1, Matrix),
    length(StartXFiltered, AuxSize),
    fill_matrix(StartXFiltered, StartYFiltered, LengthsFiltered, AuxSize, Matrix, FilledMatrix),
    complete_matrix(Matrix, LinesLength, LinesLength).

% -------------------------------------------------------------------------




createRectangles(_, _, _, _, NewRectangles, NewStartX, NewStartY, NewLengths, 1, FixedSize) :-
    NewRectangles = [rect(Ax, L1, Ay, L1, a)], 
    NewStartX = [Ax], 
    NewStartY = [Ay], 
    NewLengths = [L1], 
    Ax + L1 #=< (FixedSize+1),
    Ay + L1 #=< (FixedSize+1)
.

createRectangles(Rectangles, StartX, StartY, Lengths, ResultRectangles, ResultStartX, ResultStartY, ResultLengths, Size, FixedSize) :-
    %write(Size), nl,
    NewSize is Size - 1,
    createRectangles(Rectangles, StartX, StartY, Lengths, NewRectangles, NewStartX, NewStartY, NewLengths, NewSize, FixedSize),
    append(NewRectangles, [rect(Ax, L1, Ay, L1, a)], ResultRectangles),
    append(NewStartX, [Ax], ResultStartX),
    append(NewStartY, [Ay], ResultStartY),
    append(NewLengths, [L1], ResultLengths),
    Ax + L1 #=< (FixedSize+1),
    Ay + L1 #=< (FixedSize+1).


size(LinesLength, Size) :-
    Flag is LinesLength mod 2,
    Flag == 0,
    Size is (LinesLength // 2) * (LinesLength // 2).
size(LinesLength, Size) :-
    Flag is LinesLength mod 2,
    Flag == 1,
    Size is ((LinesLength + 1)//2) * ((LinesLength + 1)//2).


%lines(Coordenates, Lengths, N)
lines(_, _, _, []).
lines(Coordenates, Lengths, LineNo, [LineTotal|RestTotals]):-
    check_line(Coordenates, Lengths, LineNo, Counter),
    LineNo2 is LineNo + 1,
    Counter #= LineTotal,
    lines(Coordenates, Lengths, LineNo2, RestTotals).

check_line([], [], _, 0).
check_line([X|RestX], [L|RestL], LineNo, Counter):-
    LineNo #>= X #/\  LineNo #< (X + L) #<=> B,
    Counter #= Counter2 + (B*L),
    check_line(RestX,RestL, LineNo, Counter2).
