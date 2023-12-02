% Define the 2D board
% make_2d_board(0, _, []).
% make_2d_board(Rows, Cols, [Row | Rest]) :-
%     make_row(Cols, Row),
%     Rows1 is Rows - 1,
%     make_2d_board(Rows1, Cols, Rest).

% % Define a row
% make_row(0, []).
% make_row(Cols, ['O' | Rest]) :-
%     Cols1 is Cols - 1,
%     make_row(Cols1, Rest).

% Define the 2D board
make_2d_board(Board) :-
    create_rows(19, Board).

% Define rows
create_rows(0, []).
create_rows(Rows, [Row | Rest]) :-
    Rows > 0,
    Rows2 is Rows - 1,
    init_row(Row, 19),
    create_rows(Rows2, Rest).

% Initialize a row
init_row(['O','O','O','O','O','O','O','O','O','O','O','O','O','O','O','O','O','O','O'],_).


% Print a 1D row
print_1d_row([]).
print_1d_row([Cell | Rest]) :-
    format('~a ', [Cell]),
    print_1d_row(Rest).

% Print column labels
print_column_labels([]).
print_column_labels([Col | Rest]) :-
    format('~3a  ', [Col]),
    print_column_labels(Rest).

% Print the entire 2D board
print_2d_board(Board) :-
    ColumnLabels = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S'],
    format('~3a ', ['   ']),
    print_column_labels(ColumnLabels),
    format('~n'),
    print_board_rows(Board, 19, 1).

% Print the rows of the board
print_board_rows([], _, _).
print_board_rows([Row | Rest], RowLabel, StartRow) :-
    format('~2a  ', [RowLabel]),
    print_board_row_contents(Row, StartRow),
    format('~n'),
    NextRowLabel is RowLabel - 1,
    NextStartRow is StartRow + 1,
    print_board_rows(Rest, NextRowLabel, NextStartRow).

% Print the contents of a row
print_board_row_contents([], _).
print_board_row_contents([Cell | Rest], RowLabel) :-
    (Cell = 'O' -> format('~3a  ', ['.']);
    Cell = o -> format('~3a  ', ['.']);
    Cell = 'B' -> format('~3a  ', ['B']);
    Cell = b -> format('~3a  ', ['B']);
    Cell = 'W' -> format('~3a  ', ['W']);
    Cell = w -> format('~3a  ', ['W']);
    

    
     format('~3a  ', [Cell])),
    print_board_row_contents(Rest, RowLabel).

% % Set a value on the board
% set_board_value(Board, Row, Col, NewValue, UpdatedBoard) :-
%     (   Row >= 0, Row =< 18, Col >= 0, Col =< 18
%     ->  update_board(Board, Row, Col, NewValue, UpdatedBoard)
%     ;   format('Indices out of bounds.(from set_board_value)~n')
    
%     ).

% Place a stone on the board
place_stone(Board, Row, Col, Symbol, NewBoard) :-
    (Row >= 0, Row =< 18, Col >= 0, Col =< 18 ->
        (get_board_value(Board, Row, Col, Value),
        (Value = 0 ->
            update_board(Board, Row, Col, Symbol, NewBoard);
            (Value = 'O' ->
                update_board(Board, Row, Col, Symbol, NewBoard);
                (write('Invalid move.'), nl, write('Please try again.'), nl, fail)
            )
        ));
        format('Indices out of bounds.(from place_stone)~n')
    ).

% Update a specific cell on the board
% update_board([], _, _, _, []).
% update_board([FirstRow | Rest], 0, Col, NewValue, [NewRow | Rest]) :-
%     update_row(FirstRow, Col, NewValue, NewRow).
% update_board([FirstRow | Rest], Row, Col, NewValue, [FirstRow | NewRest]) :-
%     Row > 0,
%     Row1 is Row - 1,
%     update_board(Rest, Row1, Col, NewValue, NewRest).

% % Update a specific cell in a row
% update_row([], _, _, []).
% update_row([First | Rest], 0, NewValue, [NewValue | Rest]).
% update_row([First | Rest], Col, NewValue, [First | NewRest]) :-
%     Col > 0,
%     Col1 is Col - 1,
%     update_row(Rest, Col1, NewValue, NewRest).


% Base case: if the board is empty, return an empty board
update_board([], _, _, _, []).

% Case: update the first row of the board
update_board([Row|Rest], 0, Col, NewValue, [UpdatedRow|Rest]) :-
    update_row(Row, Col, NewValue, UpdatedRow).

% Case: update a row in the board
update_board([Row|Rest], RowIndex, Col, NewValue, [Row|UpdatedRest]) :-
    RowIndex > 0,
    NextIndex is RowIndex - 1,
    update_board(Rest, NextIndex, Col, NewValue, UpdatedRest).

% Base case: if the row is empty, return an empty row
update_row([], _, _, []).

% Case: update the first element of the row
update_row([_|Rest], 0, NewValue, [NewValue|Rest]).

% Case: update an element in the row
update_row([X|Rest], Col, NewValue, [X|UpdatedRest]) :-
    Col > 0,
    NextCol is Col - 1,
    update_row(Rest, NextCol, NewValue, UpdatedRest).

% set_board_value/4 predicate
set_board_value(Board, Row, Col, NewValue,UpdatedBoard) :-
    % (   Row >= 0,
    %     Row =< 18,
    %     Col >= 0,
    %     Col =< 18,
    %     update_board(Board, Row, Col, NewValue, UpdatedBoard),
    % ;
    %     format('Indices out of bounds. from set board value~n'),
    %     write('Row is '), write(Row), nl,
    %      write('Col is '), write(Col), nl
    % )
    (Row >= 0, Row =< 18, Col >= 0, Col =< 18 ->
        update_board(Board, Row, Col, NewValue, UpdatedBoard)
        
        ;
        format('Indices out of bounds.(from get_board_value)~n')
        % write('Row is '), write(Row), nl,
        % write('Col is '), write(Col), nl
    ).


% Get the value of a specific cell on the board
get_board_value(Board, Row, Col, Value) :-
    (Row >= 0, Row =< 18, Col >= 0, Col =< 18 ->
        nth0(Row, Board, SelectedRow),
        nth0(Col, SelectedRow, Value)
        
        ;
        format('Indices out of bounds.(from get_board_value)~n')
        % write('Row is '), write(Row), nl,
        % write('Col is '), write(Col), nl
    ).


% Determine if there are five consecutive occurrences of a given symbol from a starting cell in any direction.
check_five(Board, Symbol, [X, Y]) :-
    directions(Directions),
    check_both_directions(Board, Symbol, X, Y, Directions, Found),
    Found = true.

check_both_directions(_, _, _, _, [], false).

check_both_directions(Board, Symbol, X, Y, [[Dx, Dy] | Rest], Found) :-
    opposite_direction([Dx, Dy], [OppositeDx, OppositeDy]),
    check_consecutive(Board, X, Y, Dx, Dy, Symbol, 0, Count1),
    check_consecutive(Board, X, Y, OppositeDx, OppositeDy, Symbol, 0, Count2),
    Total is Count1 + Count2 ,
    (   Total >= 4
    ->  Found = true
    ;   check_both_directions(Board, Symbol, X, Y, Rest, Found)
    ).

check_consecutive(Board, X, Y, Dx, Dy, Symbol, Depth, Count) :-
    NewX is X + Dx,
    NewY is Y + Dy,
    (
        % Base case: out of bounds, max depth, or cell doesn't match the symbol
        not(is_within_bounds(NewX, NewY));
        Depth >= 5;
        (is_within_bounds(NewX,NewY),
        (get_board_value(Board, NewX, NewY, Cell), Cell \= Symbol))
    ),
    Count = 0.

check_consecutive(Board, X, Y, Dx, Dy, Symbol, Depth, Count) :-
    NewX is X + Dx,
    NewY is Y + Dy,
    is_within_bounds(NewX, NewY),
    Depth < 5,
    get_board_value(Board, NewX, NewY, Cell),
    Cell == Symbol,
    % Recursive case: continue counting in the given direction
    NextDepth is Depth + 1,
    check_consecutive(Board, NewX, NewY, Dx, Dy, Symbol, NextDepth, NextCount),
    Count is 1 + NextCount.


directions(Directions) :-
    Directions = [[1, 0], [0, 1], [1, 1], [1, -1]].

% Calculate the opposite direction
opposite_direction([Dx, Dy], [OppositeDx, OppositeDy]) :-
    OppositeDx is -Dx,
    OppositeDy is -Dy.

is_within_bounds(Row, Col) :-
    Row >= 0,
    Row < 19,
    Col >= 0,
    Col < 19.




% Check for four consecutive symbols
% check_four(Board, Row, Col, Symbol) :- check_consecutive(Board, Row, Col, Symbol, 4).
check_four(Board, Symbol, [X, Y]) :-
    directions(Directions),
    check_both_directions_four(Board, Symbol, X, Y, Directions, Found),
    Found = true.

check_both_directions_four(_, _, _, _, [], false).

check_both_directions_four(Board, Symbol, X, Y, [[Dx, Dy] | Rest], Found) :-
    opposite_direction([Dx, Dy], [OppositeDx, OppositeDy]),
    check_consecutive_four(Board, X, Y, Dx, Dy, Symbol, 0, Count1),
    check_consecutive_four(Board, X, Y, OppositeDx, OppositeDy, Symbol, 0, Count2),
    Total is Count1 + Count2 ,
    (   Total >= 3
    ->  Found = true
    ;   check_both_directions_four(Board, Symbol, X, Y, Rest, Found)
    ).

check_consecutive_four(Board, X, Y, Dx, Dy, Symbol, Depth, Count) :-
    NewX is X + Dx,
    NewY is Y + Dy,
    (
        % Base case: out of bounds, max depth, or cell doesn't match the symbol
        not(is_within_bounds(NewX, NewY));
        Depth >= 4;
        (is_within_bounds(NewX,NewY),
        (get_board_value(Board, NewX, NewY, Cell), Cell \= Symbol))
    ),
    Count = 0.

check_consecutive_four(Board, X, Y, Dx, Dy, Symbol, Depth, Count) :-
    NewX is X + Dx,
    NewY is Y + Dy,
    is_within_bounds(NewX, NewY),
    Depth < 4,
    get_board_value(Board, NewX, NewY, Cell),
    Cell == Symbol,
    % Recursive case: continue counting in the given direction
    NextDepth is Depth + 1,
    check_consecutive_four(Board, NewX, NewY, Dx, Dy, Symbol, NextDepth, NextCount),
    Count is 1 + NextCount.




opponent_symbol('W', 'B').
opponent_symbol('B', 'W').

% Check for capture
check_capture(Board, Row, Col, Symbol, CaptureBoard) :-
    opponent_symbol(Symbol, OpponentSymbol),
    capture_pair(Board, Row, Col, 0, 1, Symbol, OpponentSymbol, 2, CaptureBoard);
    capture_pair(Board, Row, Col, 0, -1, Symbol, OpponentSymbol, 2, CaptureBoard);
    capture_pair(Board, Row, Col, 1, 0, Symbol, OpponentSymbol, 2, CaptureBoard);
    capture_pair(Board, Row, Col, -1, 0, Symbol, OpponentSymbol, 2, CaptureBoard);
    capture_pair(Board, Row, Col, 1, 1, Symbol, OpponentSymbol, 2, CaptureBoard);
    capture_pair(Board, Row, Col, -1, -1, Symbol, OpponentSymbol, 2, CaptureBoard);
    capture_pair(Board, Row, Col, -1, 1, Symbol, OpponentSymbol, 2, CaptureBoard);
    capture_pair(Board, Row, Col, 1, -1, Symbol, OpponentSymbol, 2, CaptureBoard).

% Capture a pair
capture_pair(Board, X, Y, DX, DY, OColor, EColor, Count, CaptureBoard) :-
    NewX is X + DX,
    NewY is Y + DY,
    is_within_bounds(NewX, NewY),
    check_capture_direction(Board, NewX, NewY, DX, DY, OColor, EColor, Count),
    
    remove_captured(Board, NewX, NewY, DX, DY, 2, CaptureBoard).


% Check for capture in a specific direction
% Check for capture direction
% check_capture_direction(Board, X, Y, Dx, Dy, OColor, EColor, Count) :-
% (   (X >= 0, X =< 18, Y >= 0, Y =< 18)
% ->  (   Count = 0,
%         get_board_value(Board, X, Y, Cell),
%         Cell = OColor

%     ;   get_board_value(Board, X, Y, Cell),
%         Cell = EColor,
%         %print cell and EColor
%         write('Cell is '), write(Cell), nl,
%         write('EColor is '), write(EColor), nl
        

%     ;   NextX is X + Dx,
%         NextY is Y + Dy,
%         is_within_bounds(NextX, NextY),
%         NextCount is Count - 1,
%         check_capture_direction(Board, NextX, NextY, Dx, Dy, OColor, EColor, NextCount)
%     )
% ;   false
% ).

check_capture_direction(Board, X, Y, Dx, Dy, OColor, EColor, Count) :-
    (   X >= 0, X =< 18, Y >= 0, Y =< 18
    ->  (   Count = 0,
            get_board_value(Board, X, Y, Cell),
            Cell = OColor
        ;   Count > 0,
            get_board_value(Board, X, Y, Cell),
            Cell = EColor,
            Cell \= OColor,
            NextX is X + Dx,
            NextY is Y + Dy,
            is_within_bounds(NextX, NextY),
            NextCount is Count - 1,
            check_capture_direction(Board, NextX, NextY, Dx, Dy, OColor, EColor, NextCount)
        )
    ;   false
    ).



% Remove captured stones
remove_captured(Board, X, Y, DX, DY, Count, NewBoard) :-
    (Count = 0 ->
        NewBoard = Board;
        set_board_value(Board, X, Y, 'O', TempBoard),
        XNext is X + DX,
        YNext is Y + DY,
        is_within_bounds(XNext, YNext),
        NewCount is Count - 1,
        remove_captured(TempBoard, XNext, YNext, DX, DY, NewCount, NewBoard)
    ).

% Check for capture in a specific direction
recursively_check_capture(Board, Row, Col, PlayerColor, PlayerCaptures, ResultBoard, FinalCaptures) :-
    check_capture(Board, Row, Col, PlayerColor, CapturedBoard),
    (CapturedBoard \== Board ->
        NewCaptures is PlayerCaptures + 1,
        recursively_check_capture(CapturedBoard, Row, Col, PlayerColor, NewCaptures, ResultBoard, FinalCaptures)
    ;   
        ResultBoard = Board,
        FinalCaptures = PlayerCaptures
    ).

% Define the predicate to check if two positions are three points away.
is_three_points_away(InitialPos, NextPos) :-
    % Convert the numeric parts of the positions to integers.
    getRowCol(InitialPos, InitialPosRow, InitialPosCol),
    getRowCol(NextPos, NextPosRow, NextPosCol),

    % Calculate the absolute differences in rows and columns.
    rowDifference(InitialPosRow, NextPosRow, RowDiff),
    colDifference(InitialPosCol, NextPosCol, ColDiff),

    % Check if the move is at least 3 intersections away in any direction.
    (RowDiff >= 3 ; ColDiff >= 3 ; (RowDiff >= 3, ColDiff >= 3)).

% Helper predicate to extract row and column from position string.
% Helper predicate to extract row and column from position string.
getRowCol(Pos, Row, Col) :-
    atom_chars(Pos, [ColChar|RowChars]),
    char_code(ColChar, ColCode),
    ACode is 65, % ASCII code for 'A'
    Col is ColCode - ACode,
    number_chars(Row, RowChars).


% Helper predicate to calculate the absolute difference in rows.
rowDifference(Row1, Row2, Diff) :-
    Diff is abs(Row1 - Row2).

% Helper predicate to calculate the absolute difference in columns.
colDifference(Col1, Col2, Diff) :-
    Diff is abs(Col1 - Col2).


% Check for n consecutive symbols
check_n(Board, Symbol, [X, Y], N) :-
    directions(Directions),
    check_both_directions_n(Board, Symbol, X, Y, Directions, N, Found),
    Found = true.

check_both_directions_n(_, _, _, _, [], _, false).

check_both_directions_n(Board, Symbol, X, Y, [[Dx, Dy] | Rest], N, Found) :-
    opposite_direction([Dx, Dy], [OppositeDx, OppositeDy]),
    check_consecutive_n(Board, X, Y, Dx, Dy, Symbol, 0, N, Count1),
    check_consecutive_n(Board, X, Y, OppositeDx, OppositeDy, Symbol, 0, N, Count2),
    Total is Count1 + Count2 ,
    (   Total >= N-1
    ->  Found = true
    ;   check_both_directions_n(Board, Symbol, X, Y, Rest, N, Found)
    ).

check_consecutive_n(Board, X, Y, Dx, Dy, Symbol, Depth, N, Count) :-
    NewX is X + Dx,
    NewY is Y + Dy,
    (
        % Base case: out of bounds, max depth, or cell doesn't match the symbol
        not(is_within_bounds(NewX, NewY));
        Depth >= N;
        (is_within_bounds(NewX, NewY),
        (get_board_value(Board, NewX, NewY, Cell), Cell \= Symbol))
    ),
    Count = 0.

check_consecutive_n(Board, X, Y, Dx, Dy, Symbol, Depth, N, Count) :-
    NewX is X + Dx,
    NewY is Y + Dy,
    is_within_bounds(NewX, NewY),
    Depth < N,
    get_board_value(Board, NewX, NewY, Cell),
    Cell == Symbol,
    % Recursive case: continue counting in the given direction
    NextDepth is Depth + 1,
    check_consecutive_n(Board, NewX, NewY, Dx, Dy, Symbol, NextDepth, N, NextCount),
    Count is 1 + NextCount.
