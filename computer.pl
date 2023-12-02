convert_to_move(Row, Col, Move) :-
    CharCode is Col + 64,
    char_code(Char, CharCode),
    Number is 19 - Row,
    atom_concat(Char, Number, Move).


% Computer move predicate
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
            randomMove(RandomValue),
            convert_to_move(RandomValue, RandomValue, Move), % Update this line
            Result = [RandomValue, RandomValue, Move]
        ;   [Row, Col] = EvaluationResult,
            convert_to_move(Row, Col, Move),
            Result = [Row, Col, Move]
        )
    ).



secondMove(Board, Result) :-
    random(0, 19, Row),
    random(0, 19, Col),
    convertToMove(Row, Col, Move),
    (   is_three_points_away('J10', Move),
        emptyCellP(Board, Row, Col) ->
        Result = [Row, Col, Move]
    ;   secondMove(Board, Result)
    ).

% convertToMove/3 takes Row and Col and returns the move with adjusted row
% convertToMove/3 takes Row and Col and returns the move with adjusted row
convertToMove(Row, Col, Move) :-
    % Calculate column index
    ColumnIndex is Col + 65,
    
    % Convert column index to character
    char_code(CharCol, ColumnIndex),
    
    % Adjust the row by subtracting it from 19
    AdjustedRow is 19 - Row,

    % Convert adjusted row to string
    atomic_list_concat([CharCol, AdjustedRow], '', Move).


% Random move predicate
randomMove([Row, Col]) :-
    Row is random(19),
    Col is random(19).

% Check if a cell is empty
emptyCellP(Board, Row, Col) :-
    get_board_value(Board, Row, Col, Value),
    Value = 'O'.

% Find a winning move
findWinningMove(Board, PlayerSymbol, Result) :-
    checkCell(Board, 0, 0, PlayerSymbol, Result).

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
    

checkCell(CurrentBoard, Row, Col, PlayerSymbol, Result) :-
    (   emptyCellP(CurrentBoard, Row, Col) ->
        set_board_value(CurrentBoard, Row, Col, PlayerSymbol, NewBoard),
        (   check_five(NewBoard, PlayerSymbol, [Row, Col]) ->
            Result = [Row, Col] % Return the row and col of the winning move
        ; 
           checkNextCell(CurrentBoard, Row, Col, PlayerSymbol, Result)
        )
     ;  
        checkNextCell(CurrentBoard, Row, Col, PlayerSymbol, Result)
    ).

opponent_symbol('W', 'B').
opponent_symbol('B', 'W').

% Defend a winning move
defendWinningMove(Board, PlayerSymbol, Result) :-
    opponent_symbol(PlayerSymbol, OpponentSymbol),
    findWinningMove(Board, OpponentSymbol, Result).



% Find a capture position
findCapturePosition(Board, PlayerSymbol, Result) :-
    checkCellCapture(Board, 0, 0, PlayerSymbol, 0, _, _, Result).

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

checkCellCapture(CurrentBoard, Row, Col, PlayerSymbol, MaxCaptures, MaxRow, MaxCol, Result) :-
    (   emptyCellP(CurrentBoard, Row, Col) ->
        % write('Checking cell at row '), write(Row), write(' and column '), write(Col), nl,
        set_board_value(CurrentBoard, Row, Col, PlayerSymbol, NewBoard),
        (   recursively_check_capture(NewBoard, Row, Col, PlayerSymbol, MaxCaptures, CapturedBoard, Captures),
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

% Defend a capture position
defendCapturePosition(Board, PlayerSymbol, Result) :-
    opponent_symbol(PlayerSymbol, OpponentSymbol),
    findCapturePosition(Board, OpponentSymbol, Result).

% Find a move for four consecutive stones
makeFourMove(Board, PlayerSymbol, Result) :-
    checkCellFour(Board, 0, 0, PlayerSymbol, Result).

checkNextCellFour(CurrentBoard, CurrentRow, CurrentCol, PlayerSymbol, Result) :-
    (   CurrentRow >= 18 ->
        Result = [nil, nil]
        
    ;   CurrentCol >= 18 ->

        NextRow is CurrentRow + 1,
        NextCol is 0,
        checkCellFour(CurrentBoard, NextRow, NextCol, PlayerSymbol, Result)
    ;   
        % Increment the column for the next iteration
        NextCol is CurrentCol + 1,
        checkCellFour(CurrentBoard, CurrentRow, NextCol, PlayerSymbol, Result)
    ).
    

checkCellFour(CurrentBoard, Row, Col, PlayerSymbol, Result) :-
    (   emptyCellP(CurrentBoard, Row, Col) ->
        set_board_value(CurrentBoard, Row, Col, PlayerSymbol, NewBoard),
        (   check_four(NewBoard, PlayerSymbol, [Row, Col]) ->
            Result = [Row, Col] % Return the row and col of the winning move
        ; 
           checkNextCellFour(CurrentBoard, Row, Col, PlayerSymbol, Result)
        )
     ;  
        checkNextCellFour(CurrentBoard, Row, Col, PlayerSymbol, Result)
    ).



% Defend a move for four consecutive stones
defendFourMove(Board, PlayerSymbol, Result) :-
    opponent_symbol(PlayerSymbol, OpponentSymbol),
    makeFourMove(Board, OpponentSymbol, Result).

makeNMove(Board, PlayerSymbol, Consecutive, Result) :-
    checkCellN(Board, 0, 0, PlayerSymbol, Consecutive, Result).

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
    


is_nil(List):-
    List = [nil, nil].


evaluateAllCases(Board, PlayerSymbol, Result) :-
    findWinningMove(Board, PlayerSymbol, WinningMove),
    defendWinningMove(Board, PlayerSymbol, DefendingWin),
    makeFourMove(Board, PlayerSymbol, MakeFour),
    defendFourMove(Board, PlayerSymbol, DefendingFour),
    findCapturePosition(Board, PlayerSymbol, CapturingOpponent),
    defendCapturePosition(Board, PlayerSymbol, DefendingCapture),
    makeNMove(Board, PlayerSymbol, 3, MaxConsecutivePos3),
    makeNMove(Board, PlayerSymbol, 2, MaxConsecutivePos2),
    randomMove(RandomMove),

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
   
