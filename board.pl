% *********************************************************************
% make_2d_board/1
%
% Purpose: Create a 2D board for a game.
%
% Arguments:
%   - Board: The variable representing the 2D board. This will be unified with the created board.
%            (Passed by reference)
%
% Returns: None (The board is instantiated as a side effect.)
%
% Algorithm:
%   1. Call create_rows/2 to create a 2D board with 19 rows.
%   2. The create_rows/2 predicate initializes each row with 19 elements using init_row/2.
%
% Assistance Received: None
% *********************************************************************

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
init_row([o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],_).

% *********************************************************************
% print_1d_row/1
%
% Purpose: Print a 1D row of a game board.
%
% Arguments:
%   - Row: The list representing the 1D row to be printed.
%
% Returns: None
%
% Algorithm:
%   1. Recursively print each cell of the row using format/2.
%
% Assistance Received: None
% *********************************************************************
print_1d_row([]).
print_1d_row([Cell | Rest]) :-
    format('~a ', [Cell]),
    print_1d_row(Rest).

% *********************************************************************
% print_column_labels/1
%
% Purpose: Print column labels for a game board.
%
% Arguments:
%   - Columns: The list representing column labels.
%
% Returns: None
%
% Algorithm:
%   1. Recursively print each column label with a specific format using format/2.
%
% Assistance Received: None
% *********************************************************************
print_column_labels([]).
print_column_labels([Col | Rest]) :-
    format('~3a  ', [Col]),
    print_column_labels(Rest).


% *********************************************************************
% print_2d_board/1
%
% Purpose: Print the entire 2D board of a game.
%
% Arguments:
%   - Board: The 2D board to be printed.
%
% Returns: None
%
% Algorithm:
%   1. Print column labels using print_column_labels/1.
%   2. Print each row of the board with appropriate labels using print_board_rows/3.
%
% Assistance Received: None
% *********************************************************************
print_2d_board(Board) :-
    ColumnLabels = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S'],
    format('~3a ', ['   ']),
    print_column_labels(ColumnLabels),
    format('~n'),
    print_board_rows(Board, 19, 1).

% *********************************************************************
% print_board_rows/3
%
% Purpose: Print the rows of a game board.
%
% Arguments:
%   - Rows: The list of rows to be printed.
%   - RowLabel: The label for the current row.
%   - StartRow: The starting row number for printing.
%
% Returns: None
%
% Algorithm:
%   1. Print the label for the current row with appropriate formatting.
%   2. Print the contents of the row using print_board_row_contents/2.
%   3. Recursively print the remaining rows.
%
% Assistance Received: None
% *********************************************************************
print_board_rows([], _, _).
print_board_rows([Row | Rest], RowLabel, StartRow) :-
    (
        StartRow < 11 ->
        format('~2a  ', [RowLabel]);
        format('~1a   ', [RowLabel])
    ),
    print_board_row_contents(Row, StartRow),
    format('~n'),
    NextRowLabel is RowLabel - 1,
    NextStartRow is StartRow + 1,
    print_board_rows(Rest, NextRowLabel, NextStartRow).


% *********************************************************************
% print_board_row_contents/2
%
% Purpose: Print the contents of a row of a game board.
%
% Arguments:
%   - Row: The list representing the row to be printed.
%   - RowLabel: The label for the current row.
%
% Returns: None
%
% Algorithm:
%   1. Print each cell of the row with appropriate formatting based on its content.
%   2. Recursively print the remaining cells.
%
% Assistance Received: None
% *********************************************************************
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


% *********************************************************************
% place_stone/5
%
% Purpose: Place a stone on the game board at a specified position.
%
% Arguments:
%   - Board: The current game board.
%   - Row: The row index where the stone is to be placed.
%   - Col: The column index where the stone is to be placed.
%   - Symbol: The symbol representing the stone ('B' or 'W').
%   - NewBoard: The resulting game board after placing the stone.
%
% Returns: None (The NewBoard is instantiated as a side effect.)
%
% Algorithm:
%   1. Check if the given row and column indices are within bounds (0 to 18).
%   2. Retrieve the current value at the specified position on the board.
%   3. If the position is empty ('o' or 'O'), update the board with the new stone.
%   4. If the position is already occupied, display an error message and fail.
%   5. If the indices are out of bounds, display an error message.
%
% Assistance Received: None
% *********************************************************************

place_stone(Board, Row, Col, Symbol, NewBoard) :-
    (Row >= 0, Row =< 18, Col >= 0, Col =< 18 ->
        (get_board_value(Board, Row, Col, Value),
        (Value = o ->
            update_board(Board, Row, Col, Symbol, NewBoard);
            (Value = 'O' ->
                update_board(Board, Row, Col, Symbol, NewBoard);
                (write('Invalid move.'), nl, write('Please try again.'), nl, fail)
            )
        ));
        format('Indices out of bounds.(from place_stone)~n')
    ).



% *********************************************************************
% update_board/5
%
% Purpose: Update a specified cell in the game board with a new value.
%
% Arguments:
%   - Board: The current game board.
%   - RowIndex: The index of the row to be updated.
%   - Col: The index of the column to be updated.
%   - NewValue: The new value to be placed in the specified cell.
%   - UpdatedBoard: The resulting game board after the update.
%
% Returns: None (The UpdatedBoard is instantiated as a side effect.)
%
% Algorithm:
%   1. Base Case: If the board is empty, return an empty board.
%   2. Case: If RowIndex is 0, update the first row of the board using update_row/4.
%   3. Case: If RowIndex is greater than 0, update a row in the board using update_row/4.
%   4. Recursively update the remaining rows.
%
% Assistance Received: None
% *********************************************************************
update_board([], _, _, _, []).

% Case: update the first row of the board
update_board([Row|Rest], 0, Col, NewValue, [UpdatedRow|Rest]) :-
    update_row(Row, Col, NewValue, UpdatedRow).

% Case: update a row in the board
update_board([Row|Rest], RowIndex, Col, NewValue, [Row|UpdatedRest]) :-
    RowIndex > 0,
    NextIndex is RowIndex - 1,
    update_board(Rest, NextIndex, Col, NewValue, UpdatedRest).


% *********************************************************************
% update_row/4
%
% Purpose: Update a specified element in a row with a new value.
%
% Arguments:
%   - Row: The current row.
%   - Col: The index of the column to be updated.
%   - NewValue: The new value to be placed in the specified cell.
%   - UpdatedRow: The resulting row after the update.
%
% Returns: None (The UpdatedRow is instantiated as a side effect.)
%
% Algorithm:
%   1. Base Case: If the row is empty, return an empty row.
%   2. Case: If Col is 0, update the first element of the row.
%   3. Case: If Col is greater than 0, update an element in the row.
%   4. Recursively update the remaining elements in the row.
%
% Assistance Received: None
% *********************************************************************
% Base case: if the row is empty, return an empty row
update_row([], _, _, []).

% Case: update the first element of the row
update_row([_|Rest], 0, NewValue, [NewValue|Rest]).

% Case: update an element in the row
update_row([X|Rest], Col, NewValue, [X|UpdatedRest]) :-
    Col > 0,
    NextCol is Col - 1,
    update_row(Rest, NextCol, NewValue, UpdatedRest).



% *********************************************************************
% set_board_value/4
%
% Purpose: Set the value of a specific cell on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - Row: The row index of the cell to be updated.
%   - Col: The column index of the cell to be updated.
%   - NewValue: The new value to be placed in the specified cell.
%   - UpdatedBoard: The resulting game board after updating the cell.
%
% Returns: None (The UpdatedBoard is instantiated as a side effect.)
%
% Algorithm:
%   1. Check if the given row and column indices are within bounds (0 to 18).
%   2. Call the update_board/5 predicate to update the specified cell with the new value.
%   3. If the indices are out of bounds, display an error message.
%
% Assistance Received: None
% *********************************************************************
set_board_value(Board, Row, Col, NewValue,UpdatedBoard) :-
    (Row >= 0, Row =< 18, Col >= 0, Col =< 18 ->
        update_board(Board, Row, Col, NewValue, UpdatedBoard)
        
        ;
        format('Indices out of bounds.~n')
    ).


% *********************************************************************
% get_board_value/4
%
% Purpose: Get the value of a specific cell on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - Row: The row index of the cell to be retrieved.
%   - Col: The column index of the cell to be retrieved.
%   - Value: The value of the specified cell on the game board.
%
% Returns: None (The Value is instantiated as a result.)
%
% Algorithm:
%   1. Check if the given row and column indices are within bounds (0 to 18).
%   2. Use nth0/3 to retrieve the selected row.
%   3. Use nth0/3 to retrieve the value of the specified cell in the row.
%   4. If the indices are out of bounds, display an error message.
%
% Assistance Received: None
% *********************************************************************
get_board_value(Board, Row, Col, Value) :-
    (Row >= 0, Row =< 18, Col >= 0, Col =< 18 ->
        nth0(Row, Board, SelectedRow),
        nth0(Col, SelectedRow, Value)
        
        ;
        format('Indices out of bounds.~n')
    ).


% Determine if there are five consecutive occurrences of a given symbol from a starting cell in any direction.
% check_five(Board, Symbol, [X, Y]) :-
%     directions(Directions),
%     check_both_directions(Board, Symbol, X, Y, Directions, Found),
%     Found = true.

% check_both_directions(_, _, _, _, [], false).

% check_both_directions(Board, Symbol, X, Y, [[Dx, Dy] | Rest], Found) :-
%     opposite_direction([Dx, Dy], [OppositeDx, OppositeDy]),
%     check_consecutive(Board, X, Y, Dx, Dy, Symbol, 0, Count1),
%     check_consecutive(Board, X, Y, OppositeDx, OppositeDy, Symbol, 0, Count2),
%     Total is Count1 + Count2 ,
%     (   Total >= 4
%     ->  Found = true
%     ;   check_both_directions(Board, Symbol, X, Y, Rest, Found)
%     ).

% *********************************************************************
% check_consecutive/8
%
% Purpose: Check for consecutive occurrences of a symbol in a given direction on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - X: The starting row index.
%   - Y: The starting column index.
%   - Dx: The change in the row index (direction).
%   - Dy: The change in the column index (direction).
%   - Symbol: The symbol to be checked for consecutiveness.
%   - Depth: The current depth of consecutive occurrences.
%   - Count: The count of consecutive occurrences found.
%
% Returns: None (The Count is instantiated as a result.)
%
% Algorithm:
%   1. Calculate the new indices in the given direction (Dx, Dy).
%   2. Base case: Check if the new indices are out of bounds, the depth is at the maximum (5), or the cell doesn't match the symbol.
%   3. If any of the base case conditions are met, set Count to 0 and terminate.
%   4. Recursive case: If the cell matches the symbol, continue counting in the given direction.
%
% Assistance Received: None
% *********************************************************************
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


%Define the list of directions for checking consecutive occurrences on the game board.
directions(Directions) :-
    Directions = [[1, 0], [0, 1], [1, 1], [1, -1]].


% Calculate the opposite direction
opposite_direction([Dx, Dy], [OppositeDx, OppositeDy]) :-
    OppositeDx is -Dx,
    OppositeDy is -Dy.


% Check if a given row and column are within bounds 
is_within_bounds(Row, Col) :-
    Row >= 0,
    Row < 19,
    Col >= 0,
    Col < 19.




% Check for four consecutive symbols
% check_four(Board, Row, Col, Symbol) :- check_consecutive(Board, Row, Col, Symbol, 4).
% check_four(Board, Symbol, [X, Y]) :-
%     directions(Directions),
%     check_both_directions_four(Board, Symbol, X, Y, Directions, Found),
%     Found = true.

% check_both_directions_four(_, _, _, _, [], false).

% check_both_directions_four(Board, Symbol, X, Y, [[Dx, Dy] | Rest], Found) :-
%     opposite_direction([Dx, Dy], [OppositeDx, OppositeDy]),
%     check_consecutive_four(Board, X, Y, Dx, Dy, Symbol, 0, Count1),
%     check_consecutive_four(Board, X, Y, OppositeDx, OppositeDy, Symbol, 0, Count2),
%     Total is Count1 + Count2 ,
%     (   Total >= 3
%     ->  Found = true
%     ;   check_both_directions_four(Board, Symbol, X, Y, Rest, Found)
%     ).

% check_consecutive_four(Board, X, Y, Dx, Dy, Symbol, Depth, Count) :-
%     NewX is X + Dx,
%     NewY is Y + Dy,
%     (
%         % Base case: out of bounds, max depth, or cell doesn't match the symbol
%         not(is_within_bounds(NewX, NewY));
%         Depth >= 4;
%         (is_within_bounds(NewX,NewY),
%         (get_board_value(Board, NewX, NewY, Cell), Cell \= Symbol))
%     ),
%     Count = 0.

% check_consecutive_four(Board, X, Y, Dx, Dy, Symbol, Depth, Count) :-
%     NewX is X + Dx,
%     NewY is Y + Dy,
%     is_within_bounds(NewX, NewY),
%     Depth < 4,
%     get_board_value(Board, NewX, NewY, Cell),
%     Cell == Symbol,
%     % Recursive case: continue counting in the given direction
%     NextDepth is Depth + 1,
%     check_consecutive_four(Board, NewX, NewY, Dx, Dy, Symbol, NextDepth, NextCount),
%     Count is 1 + NextCount.


%Rules for checking opposite symbol
opponent_symbol(w, b).
opponent_symbol(b, w).


% *********************************************************************
% check_capture/5
%
% Purpose: Check for possible captures in all directions from a specified position on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - Row: The row index of the specified position.
%   - Col: The column index of the specified position.
%   - Symbol: The symbol representing the player.
%   - CaptureBoard: The resulting game board after potential captures.
%
% Returns: None (The CaptureBoard is instantiated as a result.)
%
% Algorithm:
%   1. Get the opponent symbol based on the player's symbol.
%   2. Attempt captures in all directions using the capture_pair/4 predicate.
%   3. If no captures are possible, set CaptureBoard to the original Board.
%
% Assistance Received: None
% *********************************************************************

check_capture(Board, Row, Col, Symbol, CaptureBoard) :-
    opponent_symbol(Symbol, OpponentSymbol),
    (
        capture_pair(Board, Row, Col, 0, 1, Symbol, OpponentSymbol, 2, CaptureBoard)
    ;
        capture_pair(Board, Row, Col, 0, -1, Symbol, OpponentSymbol, 2, CaptureBoard)
    ;
        capture_pair(Board, Row, Col, 1, 0, Symbol, OpponentSymbol, 2, CaptureBoard)
    ;
        capture_pair(Board, Row, Col, -1, 0, Symbol, OpponentSymbol, 2, CaptureBoard)
    ;
        capture_pair(Board, Row, Col, 1, 1, Symbol, OpponentSymbol, 2, CaptureBoard)
    ;
        capture_pair(Board, Row, Col, -1, 1, Symbol, OpponentSymbol, 2, CaptureBoard)
    ;
        capture_pair(Board, Row, Col, -1, -1, Symbol, OpponentSymbol, 2, CaptureBoard)
    ;
        capture_pair(Board, Row, Col, 1, -1, Symbol, OpponentSymbol, 2, CaptureBoard)
    ;
        CaptureBoard = Board
    ).
    

% *********************************************************************
% capture_pair/9
%
% Purpose: Attempt to capture stones in a specified direction from a given position on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - X: The row index of the specified position.
%   - Y: The column index of the specified position.
%   - DX: The change in the row index (direction).
%   - DY: The change in the column index (direction).
%   - OColor: The symbol representing the Own stone.
%   - EColor: The symbol representing an enemy stone.
%   - Count: The number of stones to be captured.
%   - CaptureBoard: The resulting game board after potential captures.
%
% Returns: None (The CaptureBoard is instantiated as a result.)
%
% Algorithm:
%   1. Calculate the new indices in the given direction (DX, DY).
%   2. Check for possible captures using the check_capture_direction/7 predicate.
%   3. Remove captured stones using the remove_captured/6 predicate.
%
% Assistance Received: None
% *********************************************************************
capture_pair(Board, X, Y, DX, DY, OColor, EColor, Count, CaptureBoard) :-
    NewX is X + DX,
    NewY is Y + DY,
    is_within_bounds(NewX, NewY),
    check_capture_direction(Board, NewX, NewY, DX, DY, OColor, EColor, Count),
    remove_captured(Board, NewX, NewY, DX, DY, 2, CaptureBoard).


% *********************************************************************
% check_capture_direction/8
%
% Purpose: Check for possible captures in a specific direction from a given position on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - X: The row index of the specified position.
%   - Y: The column index of the specified position.
%   - Dx: The change in the row index (direction).
%   - Dy: The change in the column index (direction).
%   - OColor: The symbol representing the player to be captured.
%   - EColor: The symbol representing the opponent's stone.
%   - Count: The count of consecutive occurrences needed for a capture.
%
% Returns: None (The predicate succeeds if a capture is possible.)
%
% Algorithm:
%   1. Check if the current position (X, Y) is within bounds.
%   2. If Count is 0, check if the current cell contains your stone (OColor).
%   3. If Count is greater than 0, check if the current cell contains the opponent's stone (EColor).
%   4. If so, recursively check the next cell in the given direction.
%
% Assistance Received: None
% *********************************************************************
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


% *********************************************************************
% remove_captured/6
%
% Purpose: Remove captured stones from the game board in a specific direction from a given position.
%
% Arguments:
%   - Board: The current game board.
%   - X: The row index of the specified position.
%   - Y: The column index of the specified position.
%   - DX: The change in the row index (direction).
%   - DY: The change in the column index (direction).
%   - Count: The count of consecutive occurrences to be removed.
%   - NewBoard: The resulting game board after removing captured stones.
%
% Returns: None (The NewBoard is instantiated as a result.)
%
% Algorithm:
%   1. Base case: If Count is 0, set NewBoard to the original Board.
%   2. Remove the stone at the current position (X, Y) by setting it to an empty cell ('o').
%   3. Calculate the next position (XNext, YNext) in the given direction.
%   4. Recursively remove the next captured stone.
%
% Assistance Received: None
% *********************************************************************
remove_captured(Board, X, Y, DX, DY, Count, NewBoard) :-
    (Count = 0 ->
        NewBoard = Board;
        set_board_value(Board, X, Y, o, TempBoard),
        XNext is X + DX,
        YNext is Y + DY,
        is_within_bounds(XNext, YNext),
        NewCount is Count - 1,
        remove_captured(TempBoard, XNext, YNext, DX, DY, NewCount, NewBoard)
    ).

% *********************************************************************
% recursively_check_capture/7
%
% Purpose: Recursively check for possible captures and update the game board and capture count.
%
% Arguments:
%   - Board: The current game board.
%   - Row: The row index of the specified position.
%   - Col: The column index of the specified position.
%   - PlayerColor: The symbol representing the player.
%   - PlayerCaptures: The current count of player's captures.
%   - ResultBoard: The resulting game board after captures.
%   - FinalCaptures: The final count of player's captures after all captures.
%
% Returns: None (The ResultBoard and FinalCaptures are instantiated as results.)
%
% Algorithm:
%   1. Call the check_capture/4 predicate to attempt captures and get the resulting board (CapturedBoard).
%   2. If CapturedBoard is different from the original Board, increment the PlayerCaptures count.
%   3. Recursively call the predicate with the updated board and count.
%   4. If no more captures are possible, set ResultBoard to the original Board and FinalCaptures to the final count.
%
% Assistance Received: None
% *********************************************************************
recursively_check_capture(Board, Row, Col, PlayerColor, PlayerCaptures, ResultBoard, FinalCaptures) :-
    check_capture(Board, Row, Col, PlayerColor, CapturedBoard),
    (CapturedBoard \= Board ->
        NewCaptures is PlayerCaptures + 1,

        recursively_check_capture(CapturedBoard, Row, Col, PlayerColor, NewCaptures, ResultBoard, FinalCaptures)
    ;   
        ResultBoard = Board,
        FinalCaptures = PlayerCaptures
    ).

% *********************************************************************
% is_three_points_away/2
%
% Purpose: Check if two positions on a game board are at least three points away from each other.
%
% Arguments:
%   - InitialPos: The position of the initial intersection.
%   - NextPos: The position of the next intersection.
%
% Returns: None (The predicate succeeds if the positions are at least three points away.)
%
% Algorithm:
%   1. Convert the positions to integers representing rows and columns.
%   2. Calculate the absolute differences in rows and columns.
%   3. Check if the move is at least 3 intersections away in any direction (row, column, or both).
%
% Assistance Received: None
% *********************************************************************
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


% *********************************************************************
% check_n/4
%
% Purpose: Check if there are N consecutive occurrences of a symbol in any direction on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - Symbol: The symbol to be checked for consecutive occurrences.
%   - [X, Y]: The position to start checking from.
%   - N: The number of consecutive occurrences to check for.
%
% Returns: None (The predicate succeeds if there are N consecutive occurrences.)
%
% Algorithm:
%   1. Get the list of directions.
%   2. Check for consecutive occurrences in both directions using the check_both_directions_n/7 predicate.
%
% Assistance Received: None
% *********************************************************************
% Check for n consecutive symbols
check_n(Board, Symbol, [X, Y], N) :-
    directions(Directions),
    check_both_directions_n(Board, Symbol, X, Y, Directions, N, Result),
    Result = true.


% *********************************************************************
% check_both_directions_n/7
%
% Purpose: Check for N consecutive occurrences of a symbol in both directions from a specified position on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - Symbol: The symbol to be checked for consecutive occurrences.
%   - X: The row index of the specified position.
%   - Y: The column index of the specified position.
%   - Directions: The list of directions represented as pairs [Dx, Dy].
%   - N: The number of consecutive occurrences to check for.
%   - Result: A flag indicating whether N consecutive occurrences are found.
%
% Returns: None (The Result flag is instantiated as a result.)
%
% Algorithm:
%   1. For each direction, check for consecutive occurrences using the check_consecutive_n/8 predicate.
%   2. If N consecutive occurrences are found in either direction, set Result to true.
%   3. Continue checking in other directions.
%
% Assistance Received: None
% *********************************************************************
check_both_directions_n(_, _, _, _, [], _, false).
check_both_directions_n(Board, Symbol, X, Y, [[Dx, Dy] | Rest], N, Result) :-
    opposite_direction([Dx, Dy], [OppositeDx, OppositeDy]),
    check_consecutive_n(Board, X, Y, Dx, Dy, Symbol, 0, N, Count1),
    check_consecutive_n(Board, X, Y, OppositeDx, OppositeDy, Symbol, 0, N, Count2),
    Total is Count1 + Count2 ,
    (   Total >= N - 1
    ->  Result = true
    ;   check_both_directions_n(Board, Symbol, X, Y, Rest, N, Result)
    ).

% *********************************************************************
% check_consecutive_n/8
%
% Purpose: Check for N consecutive occurrences of a symbol in a given direction from a specified position on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - X: The row index of the specified position.
%   - Y: The column index of the specified position.
%   - Dx: The change in the row index (direction).
%   - Dy: The change in the column index (direction).
%   - Symbol: The symbol to be checked for consecutive occurrences.
%   - Depth: The current depth of consecutive occurrences.
%   - N: The number of consecutive occurrences to check for.
%   - Count: The count of consecutive occurrences found.
%
% Returns: None (The Count is instantiated as a result.)
%
% Algorithm:
%   1. Calculate the new indices in the given direction (Dx, Dy).
%   2. Base case: Check if the new indices are out of bounds, the depth is at the maximum (N), or the cell doesn't match the symbol.
%   3. If any of the base case conditions are met, set Count to 0 and terminate.
%   4. Recursive case: If the cell matches the symbol, continue counting in the given direction.
%
% Assistance Received: None
% *********************************************************************
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


% *********************************************************************
% count_four/3
%
% Purpose: Count the total number of occurrences where there are four consecutive symbols of a given type on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - Symbol: The symbol to be checked for consecutive occurrences.
%   - Count: The total count of occurrences of four consecutive symbols.
%
% Returns: None (The Count is instantiated as a result.)
%
% Algorithm:
%   1. Call the check_four/4 predicate to check for four consecutive symbols in all directions on the board.
%
% Assistance Received: None
% *********************************************************************
count_four(Board, Symbol, Count) :-
    check_four(Board, Symbol, 0, 0, Count).

check_four(_, _, 19, _, 0).

% *********************************************************************
% check_four/5
%
% Purpose: Recursively check for four consecutive symbols in all directions on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - Symbol: The symbol to be checked for consecutive occurrences.
%   - X: The current row index in the recursion.
%   - Y: The current column index in the recursion.
%   - Count: The total count of occurrences of four consecutive symbols.
%
% Returns: None (The Count is instantiated as a result.)
%
% Algorithm:
%   1. For each cell on the board, check for four consecutive symbols in all directions using the check_four_direction/5 predicate.
%   2. Accumulate the count of occurrences.
%
% Assistance Received: None
% *********************************************************************
check_four(Board, Symbol, X, 19, Count) :-
    NewX is X + 1,
    check_four(Board, Symbol, NewX, 0, Count).

check_four(Board, Symbol, X, Y, Count) :-
    directions(Directions),
    check_four_direction(Board, Symbol, X, Y, Directions, DirectionCount),
    NewY is Y + 1,
    check_four(Board, Symbol, X, NewY, NextCount),
    Count is DirectionCount + NextCount.


% *********************************************************************
% check_four_direction/6
%
% Purpose: Check for four consecutive symbols in all directions from a specified position on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - Symbol: The symbol to be checked for consecutive occurrences.
%   - X: The row index of the specified position.
%   - Y: The column index of the specified position.
%   - Directions: The list of directions represented as pairs [Dx, Dy].
%   - Count: The total count of occurrences of four consecutive symbols in all directions.
%
% Returns: None (The Count is instantiated as a result.)
%
% Algorithm:
%   1. For each direction, check for four consecutive symbols using the check_direction_four/6 predicate.
%   2. Accumulate the count of occurrences.
%
% Assistance Received: None
% *********************************************************************
check_four_direction(_, _, _, _, [], 0).

check_four_direction(Board, Symbol, X, Y, [Dir|Rest], Count) :-
    check_direction_four(Board, Symbol, X, Y, Dir, DirectionCount),
    check_four_direction(Board, Symbol, X, Y, Rest, RestCount),
    Count is DirectionCount + RestCount.


% Check a specific direction for exactly four consecutive stones, return 1 if found, 0 otherwise.
check_direction_four(Board, Symbol, X, Y, [Dx, Dy], 1) :-
    check_consecutive(Board, X, Y, Dx, Dy, Symbol, 0, Count),
    Count == 3,
    get_board_value(Board, X, Y, Cell),
    Cell == Symbol,
    NextX is X + 4 * Dx,
    NextY is Y + 4 * Dy,
    PrevX is X - Dx,
    PrevY is Y - Dy,
    not_equal(Board, NextX, NextY, Symbol),
    not_equal(Board, PrevX, PrevY, Symbol).

check_direction_four(_, _, _, _, _, 0).

% *********************************************************************
% not_equal/4
%
% Purpose: Check if a symbol is not present at a given cell on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - X: The row index of the specified position.
%   - Y: The column index of the specified position.
%   - Symbol: The symbol to be checked for absence.
%
% Returns: None (The predicate succeeds if the symbol is not present at the specified cell.)
%
% Algorithm:
%   1. Check if the indices are out of bounds.
%   2. If the indices are within bounds, get the value at the specified cell and check if it is not equal to the given symbol.
%
% Assistance Received: None
% *********************************************************************
not_equal(Board, X, Y, Symbol) :-
    (   \+ is_within_bounds(X, Y)
    ;   is_within_bounds(X, Y),
        get_board_value(Board, X, Y, Value),
        Value \= Symbol
    ).
