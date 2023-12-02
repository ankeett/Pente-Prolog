:-compile('board.pl').

% Define a predicate to convert a character to its corresponding numerical value
char_to_num(Char, Num) :-
    char_code(Char, Code),
    Num is Code - 65. % Subtracting 65 instead of 64 to start from 0.

% Define the main predicate to convert a move to row and column
% Define the main predicate to convert a move to row and column
convertMove(Move, Row, Col) :-
    atom_chars(Move, [Char|Rest]),
    char_to_num(Char, Col),
    atomic_list_concat(Rest, RowAtom),
    atom_number(RowAtom, Row1),
    Row is 19 - Row1.% Subtracting 1 to start rows from 0.


human_move(Board, PlayerColor, MoveCount, PlayerMove) :-
    write('Player '), write(PlayerColor), write(' move: '),
    read_line_to_string(user_input, PlayerMoveString),
    atom_string(PlayerMove, PlayerMoveString),
    convertMove(PlayerMove, Row, Col),

    (valid_move(Board, [Row, Col], MoveCount)
    ->  true
    ;   (write('Invalid move'), nl, human_move(Board, PlayerColor, MoveCount, PlayerMove))
).


% Base case for the first move
getUserMove(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, 1,HumanScore, ComputerScore,Result) :-
    format('Reason: First move always at the center of the board~n'),
    Result = [9,9].

% Recursive case for subsequent moves
getUserMove(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, MoveCount, HumanScore, ComputerScore, Result) :-
    format('Enter the move (e.g., J10): ~n'),
    format('Enter HELP for a hint or QUIT for quitting the game.~n'),
    read_line_to_string(user_input, UserInput),
    format('You entered: ~w~n', [UserInput]),

    (   string_lower(UserInput, 'help')
    ->  askForHelp(Board, PlayerColor, MoveCount),
        getUserMove(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, HumanScore, ComputerScore, MoveCount, Result)
    ;   string_lower(UserInput, 'quit')
    ->  quitTheGame(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, HumanScore, ComputerScore, Result)
    ;   convertMove(UserInput, Row, Col),
        (   Row =:= -1
        ->  format('Invalid input~n'),
            getUserMove(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, MoveCount, HumanScore, ComputerScore, Result)
        ;   \+ emptyCellP(Board, Row, Col)
        ->  format('Spot already taken~n'),
            getUserMove(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, MoveCount,HumanScore, ComputerScore, Result)
        ;   (   MoveCount =:= 3,
                is_three_points_away('J10', UserInput)
            ->  Result = [Row, Col]
            ;   MoveCount \== 3
            ->  Result = [Row, Col]
            ;   format('Invalid input: Not three points away from J10.~n'),
                getUserMove(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, MoveCount,HumanScore, ComputerScore, Result)
            )
        )
    ;   % No valid input, retry
        getUserMove(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, MoveCount, HumanScore, ComputerScore,Result)
    ).
    

    
    
    

valid_move(Board, [Row, Col], MoveCount) :-
    Row >= 0,
    Row < 19,
    Col >= 0,
    Col < 19.
    


quitTheGame(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, HumanScore, ComputerScore, Result):-
    format('Quitting the game.~n'),
    Result = 'quit'.

askForHelp(Board, PlayerColor, MoveCount) :-
    computer_move(Board, PlayerColor, MoveCount, Value),
    nth0(0, Value, Row),
    nth0(1, Value, Col),
    convertToMove(Row, Col, BestMove),
    format('Best Move: ~w~n', [BestMove]).


