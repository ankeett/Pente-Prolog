:-compile('round.pl').

declare_winner(HumanScores, ComputerScores) :-
    (HumanScores > ComputerScores ->
        format('You won the tournament!~n')
    ; 
    HumanScores < ComputerScores ->
        format('Computer won the tournament!~n')
    ; 
    format('It\'s a draw!~n')
    ).


% Starting point for the tournament
tournament(HumanScore, ComputerScore) :-
    (HumanScore > ComputerScore ->
        start_round('Human', 'Computer', HumanScore, ComputerScore, Result),
        calculate_and_continue_tournament(Result, HumanScore, ComputerScore)
    ; HumanScore < ComputerScore ->
        start_round('Computer', 'Human', HumanScore, ComputerScore, Result),
        calculate_and_continue_tournament(Result, HumanScore, ComputerScore)
    ; % Draw case
        start_game(PlayerType,OpponentType),
        start_round(PlayerType,OpponentType, HumanScore, ComputerScore, Result),
        calculate_and_continue_tournament(Result, HumanScore, ComputerScore)
    ).

% Calculate scores and continue or end the tournament
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
        tournament(TotalHuman, TotalComputer)
    ; % Tournament ended
        format('Tournament ended.~n'),
        declare_winner(TotalHuman, TotalComputer)
    ).

% Predicate to read a line from input
read_line(Line) :-
    read_line_to_string(user_input, Line).

% Predicate to convert a string to lowercase
string_lower(String, Lowercase) :-
    downcase_atom(String, Lowercase).

