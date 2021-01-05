:- use_module(library(clpfd)).
:- use_module(library(lists)).

replaceInList([_H|T], 1, Value, [Value|T]).
replaceInList([H|T], Index, Value, [H|TNew]) :-
        Index > 1,
        Index1 is Index - 1,
        replaceInList(T, Index1, Value, TNew)
.

replaceInMatrix([H|T], 1, Column,Value, [HNew|T]) :-
        replaceInList(H, Column, Value, HNew)
.

replaceInMatrix([H|T], Row, Column, Value, [H|TNew]) :-
        Row > 1,
        Row1 is Row - 1,
        replaceInMatrix(T, Row1, Column, Value, TNew)
.

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

    convert(Lines, Columns, Lengths, Size, Matrix),

    write('Square Puzzle:'),nl,nl,
    displayLine(ColumnsValues),nl,
    displayMatrix(LinesValues, Matrix),nl,

    format('Time to solve: ~d milliseconds~n', [T]),nl,nl
.

displayLine([]).

displayLine([V|T]):-
    format("  ~w ",[V]),
    displayLine(T)
.

displayLine(LineValue, []):-
    format("| ~w",[LineValue])
.

displayLine(LineValue, [1|T]):-
    write('| X '),
    displayLine(LineValue, T)
.

displayLine(LineValue, [0|T]):-
    write('|   '),
    displayLine(LineValue, T)
.

displayMatrix([], []).

displayMatrix([LineValue| T1], [Line|T2]):-
    displayLine(LineValue, Line),nl,
    displayMatrix(T1,T2)
.

buildMatrix(Size, Matrix):-
    buildMatrix(Size, Size, Matrix)
.

buildMatrix(0, _ , []).

buildMatrix(Size, Length, [Line|Matrix]):-
    length(Line, Length),
    domain(Line, 0,0),
    NS is Size - 1,
    buildMatrix(NS,Length, Matrix)
.

convert(Lines, Columns, Lengths, Size, NewMatrix):-
    buildMatrix(Size, Matrix),
    convert(0, Matrix, Lines, Columns, Lengths, Size, NewMatrix)
.

convert(Size, Matrix, _, _, _, Size, Matrix).

convert(Index, Matrix, Lines, Columns, Lengths, Size, NewestMatrix):-
    nth0(Index, Lines, Line),
    nth0(Index, Columns, Column),
    nth0(Index, Lengths, Length),
    Length > 0,
    setValues(Line,Column, Length, Length, Matrix, NewMatrix),
    NewIndex is Index + 1,
    convert(NewIndex, NewMatrix, Lines, Columns, Lengths, Size, NewestMatrix)
.

convert(Index, Matrix, Lines, Columns, Lengths, Size, NewMatrix):-
    NewIndex is Index + 1,
    convert(NewIndex, Matrix, Lines, Columns, Lengths, Size, NewMatrix)
.

setLine(_, _, Matrix, 0, Matrix).

setLine(Line, Column, Matrix, Length, NewestMatrix):-
    replaceInMatrix(Matrix, Line, Column, 1, NewMatrix),
    NLen is Length - 1,
    NLin is Line + 1,
    setLine(NLin, Column, NewMatrix, NLen, NewestMatrix)
.

setValues(_, _ , 0, _, Matrix, Matrix).

setValues(Line, Column, Length, FullLength, Matrix, NewestMatrix):-
    
    setLine(Line, Column, Matrix, FullLength, NewMatrix),
    NLen is Length - 1,
    NCol is Column + 1,
    setValues(Line, NCol, NLen, FullLength, NewMatrix, NewestMatrix)
.

sortSolution([_], [_]).
sortSolution([X1,X2|T1], [Y1,Y2|T2]):-
    (X1 #= X2 #/\ Y1 #< Y2) #\/ X1 #< X2,
    sortSolution([X2|T1],[Y2|T2]),!
.

createRectangles(_, _, _, _, NewRectangles, NewLines, NewColumns, NewLengths, 1, FullSize) :-
    NewRectangles = [rectangle(X, L, Y, L, a)], 
    NewLines = [X], 
    NewColumns = [Y], 
    NewLengths = [L], 
    X + L #=< (FullSize+1),
    Y + L #=< (FullSize+1)
.

createRectangles(Rectangles, Lines, Columns, Lengths, ResultRectangles, ResultLines, ResultColumns, ResultLengths, Size, FullSize) :-
    NewSize is Size - 1,
    createRectangles(Rectangles, Lines, Columns, Lengths, NewRectangles, NewLines, NewColumns, NewLengths, NewSize, FullSize),
    append(NewRectangles, [rectangle(X, L, Y, L, a)], ResultRectangles),
    append(NewLines, [X], ResultLines),
    append(NewColumns, [Y], ResultColumns),
    append(NewLengths, [L], ResultLengths),
    X + L #=< (FullSize+1),
    Y + L #=< (FullSize+1)
.


size(LinesLength, Size) :-
    Reminder is LinesLength mod 2,
    Reminder == 0,
    Size is (LinesLength // 2) * (LinesLength // 2)
.

size(LinesLength, Size) :-
    Size is ((LinesLength + 1)//2) * ((LinesLength + 1)//2)
.

lines(_, _, _, []).
lines(Positions, Lengths, Line, [LineTotal|T]):-
    verifyLine(Positions, Lengths, Line, Count),
    LineNo2 is Line + 1,
    Count #= LineTotal,
    lines(Positions, Lengths, LineNo2, T)
.

verifyLine([], [], _, 0).
verifyLine([X|Tx], [L|TL], Line, Count):-
    Line #>= X #/\  Line #< (X + L) #<=> B,
    Count #= NewCount + (B*L),
    verifyLine(Tx,TL, Line, NewCount)
.
