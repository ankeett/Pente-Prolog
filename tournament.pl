:-compile('round.pl').

% *********************************************************************
% declare_winner/2
%
% Purpose: Declares the winner of the tournament based on the scores.
%
% Arguments:
%   - HumanScores: The total scores of the human player in the tournament.
%   - ComputerScores: The total scores of the computer player in the tournament.
%
% Returns: None (Prints the winner or a draw message to the console.)
%
% Algorithm:
%   1. Compares the total scores of the human and computer players.
%   2. Prints a message declaring the winner or indicating a draw.
%
% Assistance Received: None
% *********************************************************************
declare_winner(HumanScores, ComputerScores) :-
    (HumanScores > ComputerScores ->
        format('You won the tournament!~n')
    ; 
    HumanScores < ComputerScores ->
        format('Computer won the tournament!~n')
    ; 
    format('It\'s a draw!~n')
    ).


% *********************************************************************
% tournament/2
%
% Purpose: Initiates the Pente tournament, determines the winner, and continues or ends the tournament.
%
% Arguments:
%   - HumanScore: The total scores of the human player in the tournament.
%   - ComputerScore: The total scores of the computer player in the tournament.
%
% Returns: None (Controls the flow of the tournament, starting new rounds or declaring a winner.)
%
% Algorithm:
%   1. Compares the total scores of the human and computer players.
%   2. Starts a new round with the player who has the higher score as White.
%   3. Calculates and continues the tournament based on the round result.
%
% Assistance Received: None
% *********************************************************************
tournament(HumanScore, ComputerScore) :-
    (HumanScore > ComputerScore ->
        start_round(human, computer, HumanScore, ComputerScore, Result),
        calculate_and_continue_tournament(Result, HumanScore, ComputerScore)
    ; HumanScore < ComputerScore ->
        start_round(computer, human, HumanScore, ComputerScore, Result),
        calculate_and_continue_tournament(Result, HumanScore, ComputerScore)
    ; % Draw case
        start_game(PlayerType,OpponentType),
        start_round(PlayerType,OpponentType, HumanScore, ComputerScore, Result),
        calculate_and_continue_tournament(Result, HumanScore, ComputerScore)
    ).


% *********************************************************************
% calculate_and_continue_tournament/3
%
% Purpose: Calculates the scores based on the result of a round and decides whether to continue or end the tournament.
%
% Arguments:
%   - Result: The result of the current round, including updated scores.
%   - HumanScore: The total scores of the human player in the tournament.
%   - ComputerScore: The total scores of the computer player in the tournament.
%
% Returns: None (Controls the flow of the tournament, continuing or ending based on user input.)
%
% Algorithm:
%   1. Calculates the scores based on the result of the current round.
%   2. Adds the round scores to the total scores of the human and computer players.
%   3. Displays the updated tournament scores.
%   4. Asks the user if they want to continue the tournament.
%   5. If the user wants to continue, starts the next round.
%   6. If the user decides to end the tournament, declares the winner.
%
% Assistance Received: None
% *********************************************************************
calculate_and_continue_tournament(Result, HumanScore, ComputerScore) :-
    calculate_score(Result, HumanScoreResult, ComputerScoreResult),

    TotalHuman is HumanScore + HumanScoreResult,
    TotalComputer is ComputerScore + ComputerScoreResult, 

    format('-----Tournament Scores-----~n'),
    format('Human Scores: ~a~n', [TotalHuman]),
    format('Computer Scores: ~a~n', [TotalComputer]),
    format('Continue the tournament? (Enter y to confirm!): ~n'),
    read_line(Response),
    (string_lower(Response, 'y') ->
        write('Starting next round...'),nl,
        write('--------------------------------------------'),nl,
        tournament(TotalHuman, TotalComputer)
    ; % Tournament ended
        format('Tournament ended.~n'),
        declare_winner(TotalHuman, TotalComputer)
    ).

% *********************************************************************
% read_line/1
%
% Purpose: Reads a line from the user input.
%
% Arguments:
%   - Line: The variable to store the read line.
%
% Returns: None (Unifies the variable with the read line.)
%
% Algorithm:
%   1. Uses read_line_to_string to read a line from the user input.
%
% Assistance Received: None
% *********************************************************************
read_line(Line) :-
    read_line_to_string(user_input, Line).

% *********************************************************************
% string_lower/2
%
% Purpose: Converts a string to lowercase.
%
% Arguments:
%   - String: The input string to be converted to lowercase.
%   - Lowercase: The variable to store the lowercase version of the string.
%
% Returns: None (Unifies the variable with the lowercase version of the string.)
%
% Algorithm:
%   1. Uses downcase_atom to convert the string to lowercase.
%
% Assistance Received: None
% *********************************************************************
string_lower(String, Lowercase) :-
    downcase_atom(String, Lowercase).

