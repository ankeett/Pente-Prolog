:-compile('board.pl').

% *********************************************************************
% char_to_num/2
%
% Purpose: Convert a character to its corresponding numerical value.
%
% Arguments:
%   - Char: The input character to be converted.
%   - Num: The numerical value corresponding to the character.
%
% Returns: None (The Num is instantiated as a result.)
%
% Algorithm:
%   1. Convert the input character to uppercase.
%   2. Get the ASCII code of the uppercase character.
%   3. Subtract 65 (ASCII code for 'A') to get the numerical value.
%
% Assistance Received: None
% *********************************************************************
char_to_num(Char, Num) :-
    upcase_atom(Char, UppercaseChar),
    char_code(UppercaseChar, Code),
    Num is Code - 65. 


% *********************************************************************
% convertMove/3
%
% Purpose: Convert a move (in the format 'A1') to row and column indices.
%
% Arguments:
%   - Move: The input move in the format 'A1'.
%   - Row: The row index obtained from the move.
%   - Col: The column index obtained from the move.
%
% Returns: None (The Row and Col are instantiated as results.)
%
% Algorithm:
%   1. Extract the first character (column) and the remaining characters (row).
%   2. Use char_to_num to convert the column character to its numerical value.
%   3. Convert the remaining characters (row) to a numerical value.
%   4. Set Row as 19 minus to adjust the row value.
%
% Assistance Received: None
% *********************************************************************
convertMove(Move, Row, Col) :-
    atom_chars(Move, [Char|Rest]),
    char_to_num(Char, Col),
    atomic_list_concat(Rest, RowAtom),
    atom_number(RowAtom, Row1),
    Row is 19 - Row1.

% *********************************************************************
% getUserMove/11
%
% Purpose: Handles user input to obtain the next move in the game.
%
% Arguments:
%   - Board: The current state of the game board.
%   - PlayerColor: The color of the player making the move.
%   - PlayerType: The type of the player making the move (human or computer).
%   - OpponentColor: The color of the opponent player.
%   - OpponentType: The type of the opponent player (human or computer).
%   - PlayerCaptures: The number of stones captured by the player.
%   - OpponentCaptures: The number of stones captured by the opponent.
%   - MoveCount: The current count of moves in the game.
%   - HumanScore: The score of the human player.
%   - ComputerScore: The score of the computer player.
%   - Result: The result, i.e., the next move [Row, Col] obtained from user input.
%
% Returns: None (The Result is instantiated as the next move coordinates.)
%
% Algorithm:
%   1. Prompts the user to enter the next move.
%   2. Handles special cases for the first move.
%   3. Validates user input and ensures the move is valid.
%   4. If the user requests help, provides a hint using askForHelp/3 predicate.
%   5. If the input is 'quit', exits the game.
%   6. If the move is valid, returns the [Row, Col] of the move.
%   7. Recursively retries if the input is invalid.
%
% Assistance Received: None
% *********************************************************************

% Base case for the first move
getUserMove(_, _, _, _, _, _, _, 1,_, _,Result) :-
    format('Reason: First move always at the center of the board~n'),
    Result = [9,9].

% Recursive case for subsequent moves
getUserMove(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, MoveCount, HumanScore, ComputerScore, Result) :-
    format('Enter the move (e.g., J10): ~n'),
    format('Enter HELP for a hint.~n'),
    read_line_to_string(user_input, UserInput),
    format('You entered: ~w~n', [UserInput]),

    (   string_lower(UserInput, 'help')
    ->  askForHelp(Board, PlayerColor, MoveCount),
        getUserMove(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, HumanScore, ComputerScore, MoveCount, Result)
    % ;   string_lower(UserInput, 'quit')
    % ->    quitTheGame(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, HumanScore, ComputerScore, Result)
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
            ->  
            (valid_move(Board, [Row, Col], MoveCount)
              ->  Result = [Row, Col]
                ;   format('Invalid input: Invalid input.~n'),
                    getUserMove(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, MoveCount,HumanScore, ComputerScore, Result)
            )

            
            ;   format('Invalid input: Not three points away from J10.~n'),
                getUserMove(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, MoveCount,HumanScore, ComputerScore, Result)
            )
        )
    ;   % No valid input, retry
        getUserMove(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, MoveCount, HumanScore, ComputerScore,Result)
    ).
     

%check the boundary of the board
valid_move(_, [Row, Col], _) :-
    Row >= 0,
    Row < 19,
    Col >= 0,
    Col < 19.
    

% *********************************************************************
% askForHelp/3
%
% Purpose: Provides a hint for the next move based on the current state of the game.
%
% Arguments:
%   - Board: The current state of the game board.
%   - PlayerColor: The color of the player requesting help.
%   - MoveCount: The current count of moves in the game.
%
% Returns: None (Outputs the best move as a hint to the user.)
%
% Algorithm:
%   1. Calls the computer_move/4 predicate to obtain the best move for the player.
%   2. Extracts the row and column from the obtained move.
%   3. Converts the row and column to the move format (e.g., 'J10').
%   4. Outputs the best move as a hint to the user.
%
% Assistance Received: None
% *********************************************************************
askForHelp(Board, PlayerColor, MoveCount) :-
    computer_move(Board, PlayerColor, MoveCount, Value),
    nth0(0, Value, Row),
    nth0(1, Value, Col),
    convertToMove(Row, Col, BestMove),
    format('Best Move: ~w~n', [BestMove]).


