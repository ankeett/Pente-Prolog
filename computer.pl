
% *********************************************************************
% convert_to_move/3
%
% Purpose: Convert row and column indices to a move representation in standard Go notation.
%
% Arguments:
%   - Row: The row index of the move.
%   - Col: The column index of the move.
%   - Move: The resulting move representation in standard Go notation.
%
% Returns: None (The Move is instantiated as a result.)
%
% Algorithm:
%   1. Convert the column index to a character representation by adding 65 (ASCII code for 'A').
%   2. Concatenate the character and the complement of the row index to form the move representation.
%
% Assistance Received: None
% *********************************************************************
convert_to_move(Row, Col, Move) :-
    CharCode is Col + 65,
    char_code(Char, CharCode),
    Number is 19 - Row,
    atom_concat(Char, Number, Move).


% *********************************************************************
% computer_move/4
%
% Purpose: Determine the computer's move based on the current game board and player symbol.
%
% Arguments:
%   - Board: The current game board.
%   - PlayerSymbol: The symbol of the human player.
%   - MoveCount: The current count of moves made in the game.
%   - Result: The resulting move, represented as [Row, Col, Move].
%
% Returns: None (The Result is instantiated as a result.)
%
% Algorithm:
%   1. If it is the first move, place the stone at the center of the board (Row: 9, Col: 9).
%   2. If it is the third move, place the stone 3 intersections away from the center.
%   3. Otherwise, evaluate the game board using evaluateAllCases/3 and make a decision based on the evaluation.
%      - If the evaluation result is [nil, nil], make a random move.
%      - If the evaluation result contains [Row, Col], use the given coordinates for the move.
%
% Assistance Received: None
% *********************************************************************
computer_move(Board, PlayerSymbol, MoveCount, Result) :-
    (   MoveCount = 1 ->
        format('--------------------------------------------~n'),
        format('Reason: First stone always at the center of the board~n'),
        Row is 9,
        Col is 9,
        Move = 'J10',
        Result = [Row, Col, Move]
    ;   MoveCount = 3 ->
        format('--------------------------------------------~n'),
        format('Reason: 3 intersections away from the center of the board~n'),
        secondMove(Board, Result)
    ;   format('--------------------------------------------~n'),
        evaluateAllCases(Board, PlayerSymbol, EvaluationResult),
        (   EvaluationResult = [nil, nil] ->
            randomMove(Board,[Row,Col]),
            convert_to_move(Row, Col, Move), 
            Result = [Row, Col, Move]
        ;   [Row, Col] = EvaluationResult,
            convert_to_move(Row, Col, Move),
            Result = [Row, Col, Move]
        )
    ).


% *********************************************************************
% secondMove/2
%
% Purpose: Generate the computer's second move by placing the stone at a random empty cell three points away from the center.
%
% Arguments:
%   - Board: The current game board.
%   - Result: The resulting move, represented as [Row, Col, Move].
%
% Returns: None (The Result is instantiated as a result.)
%
% Algorithm:
%   1. Generate random row and column indices within the board bounds.
%   2. Convert the indices to a move representation using convertToMove/3.
%   3. Check if the generated move is three points away from the center and the cell is empty.
%   4. If the conditions are met, set the Result with the move; otherwise, retry.
%
% Assistance Received: None
% *********************************************************************
secondMove(Board, Result) :-
    random(0, 19, Row),
    random(0, 19, Col),
    convertToMove(Row, Col, Move),
    (   is_three_points_away('J10', Move),
        emptyCellP(Board, Row, Col) ->
        Result = [Row, Col, Move]
    ;   secondMove(Board, Result)
    ).

% *********************************************************************
% convertToMove/3
%
% Purpose: Convert row and column indices to a move representation in standard Go notation with an adjusted row.
%
% Arguments:
%   - Row: The row index of the move.
%   - Col: The column index of the move.
%   - Move: The resulting move representation in standard Pente notation.
%
% Returns: None (The Move is instantiated as a result.)
%
% Algorithm:
%   1. Calculate the column index by adding 65 (ASCII code for 'A').
%   2. Convert the column index to a character representation.
%   3. Adjust the row by subtracting it from 19.
%   4. Concatenate the character and the adjusted row to form the move representation.
%
% Assistance Received: None
% *********************************************************************
convertToMove(Row, Col, Move) :-
    % Calculate column index
    ColumnIndex is Col + 65,
    
    % Convert column index to character
    char_code(CharCol, ColumnIndex),
    
    % Adjust the row by subtracting it from 19
    AdjustedRow is 19 - Row,

    % Convert adjusted row to string
    atomic_list_concat([CharCol, AdjustedRow], '', Move).


% *********************************************************************
% randomMove/2
%
% Purpose: Generate a random move on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - [Row, Col]: The resulting random move represented by row and column indices.
%
% Returns: None (The [Row, Col] is instantiated as a result.)
%
% Algorithm:
%   1. Generate random row and column indices within the board bounds.
%   2. Check if the generated cell is empty. If not, generate another random move.
%
% Assistance Received: None
% *********************************************************************
randomMove(Board,[Row, Col]) :-
    Row is random(19),
    Col is random(19),

    %check if the cell is empty, if not empty then generate another random move
    (emptyCellP(Board, Row, Col)
    -> true;
    randomMove(Board,[Row, Col])
        ).

% Check if a cell is empty
emptyCellP(Board, Row, Col) :-
    get_board_value(Board, Row, Col, Value),
    Value = o.

% *********************************************************************
% findWinningMove/3
%
% Purpose: Find a winning move for the given player symbol on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - PlayerSymbol: The symbol of the player for whom a winning move is being searched.
%   - Result: The resulting winning move, represented as [Row, Col].
%
% Returns: None (The Result is instantiated as a result.)
%
% Algorithm:
%   1. Start checking each cell on the board.
%   2. If an empty cell is found, set the cell with the player symbol.
%   3. Check if the move results in five consecutive symbols in any direction.
%   4. If yes, set Result as [Row, Col] of the winning move; otherwise, continue checking the next cell.
%
% Assistance Received: None
% *********************************************************************
findWinningMove(Board, PlayerSymbol, Result) :-
    checkCell(Board, 0, 0, PlayerSymbol, Result).

%helper function to check the next cell
checkNextCell(CurrentBoard, CurrentRow, CurrentCol, PlayerSymbol, Result) :-
    (   CurrentRow >= 18 ->
        Result = [nil, nil]
        
    ;   CurrentCol >= 18 ->
        NextRow is CurrentRow + 1,
        NextCol is 0,
        checkCell(CurrentBoard, NextRow, NextCol, PlayerSymbol, Result)
    ;   
        % Increment the column for the next iteration
        NextCol is CurrentCol + 1,
        checkCell(CurrentBoard, CurrentRow, NextCol, PlayerSymbol, Result)
    ).
    

%helper function to check the cell
checkCell(CurrentBoard, Row, Col, PlayerSymbol, Result) :-
    (   emptyCellP(CurrentBoard, Row, Col) ->
        set_board_value(CurrentBoard, Row, Col, PlayerSymbol, NewBoard),
        (   check_n(NewBoard, PlayerSymbol, [Row, Col],5) ->
            Result = [Row, Col] % Return the row and col of the winning move
        ; 
           checkNextCell(CurrentBoard, Row, Col, PlayerSymbol, Result)
        )
     ;  
        checkNextCell(CurrentBoard, Row, Col, PlayerSymbol, Result)
    ).



% *********************************************************************
% defendWinningMove/3
%
% Purpose: Defend against a winning move by the opponent on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - PlayerSymbol: The symbol of the player who needs to defend.
%   - Result: The resulting move to defend against a winning move.
%
% Returns: None (The Result is instantiated as a result.)
%
% Algorithm:
%   1. Determine the opponent's symbol.
%   2. Make a move to prevent the opponent from winning by using makeNMove/3 with N = 5.
%   3. Set Result as the resulting move to defend against a winning move.
%
% Assistance Received: None
% *********************************************************************
defendWinningMove(Board, PlayerSymbol, Result) :-
    opponent_symbol(PlayerSymbol, OpponentSymbol),
    makeNMove(Board, OpponentSymbol,5, Result).


% *********************************************************************
% findCapturePosition/3
%
% Purpose: Find a position on the game board where capturing opponent stones results in the maximum captures.
%
% Arguments:
%   - Board: The current game board.
%   - PlayerSymbol: The symbol of the player attempting to find a capture position.
%   - Result: The resulting position where capturing opponent stones results in the maximum captures, represented as [Row, Col].
%
% Returns: None (The Result is instantiated as a result.)
%
% Algorithm:
%   1. Start checking each cell on the board.
%   2. If an empty cell is found, set the cell with the player symbol.
%   3. Use recursively_check_capture/7 to check the captures after placing the stone.
%   4. If the captures are greater than the current maximum captures, update MaxCaptures and the position.
%   5. Continue checking the next cell.
%
% Assistance Received: None
% *********************************************************************

% Find a capture position
findCapturePosition(Board, PlayerSymbol, Result) :-
    checkCellCapture(Board, 0, 0, PlayerSymbol, 0, _, _, Result).

% Check the next cell for a capture position
checkNextCellCapture(CurrentBoard, CurrentRow, CurrentCol, PlayerSymbol, MaxCaptures, MaxRow, MaxCol, Result) :-
    (   CurrentRow >= 18 ->
        Result = [MaxRow, MaxCol]
    ;   CurrentCol >= 18 ->
        NextRow is CurrentRow + 1,
        NextCol is 0,
        checkCellCapture(CurrentBoard, NextRow, NextCol,PlayerSymbol, MaxCaptures, MaxRow, MaxCol, Result)
    ;   
        NextCol is CurrentCol + 1,
        checkCellCapture(CurrentBoard, CurrentRow, NextCol,PlayerSymbol, MaxCaptures, MaxRow, MaxCol, Result)
    ).

% Check the cell for a capture position
checkCellCapture(CurrentBoard, Row, Col, PlayerSymbol, MaxCaptures, MaxRow, MaxCol, Result) :-
    (   emptyCellP(CurrentBoard, Row, Col) ->
        % write('Checking cell at row '), write(Row), write(' and column '), write(Col), nl,
        set_board_value(CurrentBoard, Row, Col, PlayerSymbol, NewBoard),
        (   recursively_check_capture(NewBoard, Row, Col, PlayerSymbol, 0, CapturedBoard, Captures),
            CapturedBoard \= NewBoard ->
            (   Captures > MaxCaptures ->
                checkNextCellCapture(CurrentBoard, Row, Col,PlayerSymbol, Captures, Row, Col, Result)
            ;   checkNextCellCapture(CurrentBoard, Row, Col,PlayerSymbol, MaxCaptures, MaxRow, MaxCol, Result)
            )
        ;   checkNextCellCapture(CurrentBoard, Row, Col,PlayerSymbol, MaxCaptures, MaxRow, MaxCol, Result)
        )
    ;   
        % write('Cell is not empty~n'),nl,
    checkNextCellCapture(CurrentBoard, Row, Col,PlayerSymbol, MaxCaptures, MaxRow, MaxCol, Result)
    ).


% *********************************************************************
% defendCapturePosition/3
%
% Purpose: Defend against a capture position by the opponent on the game board.
%
% Arguments:
%   - Board: The current game board.
%   - PlayerSymbol: The symbol of the player who needs to defend.
%   - Result: The resulting position to defend against a capture position, represented as [Row, Col].
%
% Returns: None (The Result is instantiated as a result.)
%
% Algorithm:
%   1. Determine the opponent's symbol.
%   2. Find a capture position by the opponent using findCapturePosition/3.
%   3. Set Result as the resulting position to defend against a capture position.
%
% Assistance Received: None
% *********************************************************************
defendCapturePosition(Board, PlayerSymbol, Result) :-
    opponent_symbol(PlayerSymbol, OpponentSymbol),
    findCapturePosition(Board, OpponentSymbol, Result).

% % Find a move for four consecutive stones
% makeFourMove(Board, PlayerSymbol, Result) :-
%     checkCellFour(Board, 0, 0, PlayerSymbol, Result).

% checkNextCellFour(CurrentBoard, CurrentRow, CurrentCol, PlayerSymbol, Result) :-
%     (   CurrentRow >= 18 ->
%         Result = [nil, nil]
        
%     ;   CurrentCol >= 18 ->

%         NextRow is CurrentRow + 1,
%         NextCol is 0,
%         checkCellFour(CurrentBoard, NextRow, NextCol, PlayerSymbol, Result)
%     ;   
%         % Increment the column for the next iteration
%         NextCol is CurrentCol + 1,
%         checkCellFour(CurrentBoard, CurrentRow, NextCol, PlayerSymbol, Result)
%     ).
    

% checkCellFour(CurrentBoard, Row, Col, PlayerSymbol, Result) :-
%     (   emptyCellP(CurrentBoard, Row, Col) ->
%         set_board_value(CurrentBoard, Row, Col, PlayerSymbol, NewBoard),
%         (   check_n(NewBoard, PlayerSymbol, [Row, Col],4) ->
%             Result = [Row, Col] % Return the row and col of the winning move
%         ; 
%            checkNextCellFour(CurrentBoard, Row, Col, PlayerSymbol, Result)
%         )
%      ;  
%         checkNextCellFour(CurrentBoard, Row, Col, PlayerSymbol, Result)
%     ).



% Defend a move for four consecutive stones
defendFourMove(Board, PlayerSymbol, Result) :-
    opponent_symbol(PlayerSymbol, OpponentSymbol),
    makeNMove(Board, OpponentSymbol,4, Result).




% *********************************************************************
% makeNMove/4
%
% Purpose: Make a move on the game board where placing the stone results in N consecutive symbols for the player.
%
% Arguments:
%   - Board: The current game board.
%   - PlayerSymbol: The symbol of the player attempting to make a move.
%   - Consecutive: The number of consecutive symbols required.
%   - Result: The resulting move to achieve N consecutive symbols, represented as [Row, Col].
%
% Returns: None (The Result is instantiated as a result.)
%
% Algorithm:
%   1. Start checking each cell on the board.
%   2. If an empty cell is found, set the cell with the player symbol.
%   3. Use check_n/4 to check if placing the stone results in N consecutive symbols for the player.
%   4. If true, set Result as the position to achieve N consecutive symbols.
%   5. Continue checking the next cell.
%
% Assistance Received: None
% *********************************************************************
makeNMove(Board, PlayerSymbol, Consecutive, Result) :-
    checkCellN(Board, 0, 0, PlayerSymbol, Consecutive, Result).

%helper function to check the next cell
checkNextCellN(CurrentBoard, CurrentRow, CurrentCol, PlayerSymbol, Consecutive, Result) :-
    (   CurrentRow >= 18 ->
        Result = [nil, nil]
    ;   CurrentCol >= 18 ->
        NextRow is CurrentRow + 1,
        NextCol is 0,
        checkCellN(CurrentBoard, NextRow, NextCol, PlayerSymbol, Consecutive, Result)
    ;   
        % Increment the column for the next iteration
        NextCol is CurrentCol + 1,
        checkCellN(CurrentBoard, CurrentRow, NextCol, PlayerSymbol, Consecutive, Result)
    ).

%helper function to check the cell for N consecutive symbols
checkCellN(CurrentBoard, Row, Col, PlayerSymbol, Consecutive, Result) :-
    (   emptyCellP(CurrentBoard, Row, Col) ->
        set_board_value(CurrentBoard, Row, Col, PlayerSymbol, NewBoard),
        (   check_n(NewBoard, PlayerSymbol, [Row, Col], Consecutive) ->
            Result = [Row, Col] % Return the row and col of the winning move
        ; 
            checkNextCellN(CurrentBoard, Row, Col, PlayerSymbol, Consecutive, Result)
        )
        ;  
        checkNextCellN(CurrentBoard, Row, Col, PlayerSymbol, Consecutive, Result)
    ).
    


% check if the list is nil
is_nil(List):-
    List = [nil, nil].


% *********************************************************************
% evaluateAllCases/3
%
% Purpose: Evaluate various cases to determine the best move for the player.
%
% Arguments:
%   - Board: The current game board.
%   - PlayerSymbol: The symbol of the player.
%   - Result: The resulting move determined based on the evaluation, represented as [Row, Col].
%
% Returns: None (The Result is instantiated as a result.)
%
% Algorithm:
%   1. Evaluate different cases in the following order:
%      - Make a move to achieve 5 consecutive symbols.
%      - Defend against the opponent's winning move.
%      - Make a move to achieve 4 consecutive symbols.
%      - Defend against the opponent's attempt to achieve 4 consecutive symbols.
%      - Find a position to capture the opponent.
%      - Defend against the opponent's attempt to capture.
%      - Make a move to achieve 3 consecutive symbols.
%      - Make a move to achieve 2 consecutive symbols.
%      - Generate a random move.
%   2. Set Result based on the case that matches.
%
% Assistance Received: None
% *********************************************************************
evaluateAllCases(Board, PlayerSymbol, Result) :-
    makeNMove(Board, PlayerSymbol, 5, WinningMove),
    defendWinningMove(Board, PlayerSymbol, DefendingWin),
    makeNMove(Board, PlayerSymbol, 4, MakeFour),
    defendFourMove(Board, PlayerSymbol, DefendingFour),
    findCapturePosition(Board, PlayerSymbol, CapturingOpponent),
    defendCapturePosition(Board, PlayerSymbol, DefendingCapture),
    makeNMove(Board, PlayerSymbol, 3, MaxConsecutivePos3),
    makeNMove(Board, PlayerSymbol, 2, MaxConsecutivePos2),
    randomMove(Board,RandomMove),

    (   \+ is_nil(WinningMove) ->
        format('Reason: Winning Move~n'),
        Result = WinningMove
    ;   \+ is_nil(DefendingWin) ->
        format('Reason: Defending Win~n'),
        Result = DefendingWin
    ;   \+ is_nil(DefendingFour) ->
        format('Reason: Defending Four~n'),
        Result = DefendingFour
    ;   \+ is_nil(CapturingOpponent) ->
        format('Reason: Capturing Opponent~n'),
        Result = CapturingOpponent
    ;   \+ is_nil(DefendingCapture) ->
        format('Reason: Defending Capture~n'),
        Result = DefendingCapture
    ;   \+ is_nil(MakeFour) ->
        format('Reason: Making Four~n'),
        Result = MakeFour
    ;   \+ is_nil(MaxConsecutivePos3) ->
        format('Reason: Max Consecutive of 3~n'),
        Result = MaxConsecutivePos3
    ;   \+ is_nil(MaxConsecutivePos2) ->
        format('Reason: Max Consecutive of 2~n'),
        Result = MaxConsecutivePos2
    ;   format('Reason: Random Move~n'),
        Result = RandomMove
    ).
   
