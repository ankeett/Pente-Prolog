:-compile('tournament.pl').
:-compile('serialization.pl').


% *********************************************************************
% welcome/1
%
% Purpose: Displays a welcome message and prompts the user to start a new game or load a saved game.
%
% Arguments:
%   - Result: The user's choice (1 for starting a new game, 2 for loading a saved game).
%
% Returns: None (Outputs messages to the user and obtains user input.)
%
% Algorithm:
%   1. Displays a welcome message to the user.
%   2. Prompts the user to choose between starting a new game or loading a saved game.
%   3. Obtains and validates the user's choice.
%   4. Returns the user's choice as the result.
%
% Assistance Received: None
% *********************************************************************
welcome(Result) :-
    write('Welcome to Pente!'), nl,
    write('You will be playing against the computer.'), nl,
    write('Choose:'), nl,
    write('1. Start a new game'), nl,
    write('2. Load a saved game'), nl,
    get_welcome_input(Result).

%helper function for welcome predicate to get user input
get_welcome_input(Result) :-
    write('Enter your choice (1 or 2): '),
    read_line_to_string(user_input, ChoiceStr),
    (
        (
            atom_number(ChoiceStr, Choice)
            ->
            (Choice = 1 -> Result = 1;
            Choice = 2 -> Result = 2;
            write('Invalid choice. Try again.'), nl,
            get_welcome_input(Result))
            ;
            write('Invalid choice. Try again.'), nl,
            get_welcome_input(Result)
        )
    ).

% *********************************************************************
% game/0
%
% Purpose: Initiates the Pente game. Displays a welcome message and prompts the user to start a new game or load a saved game.
%
% Arguments: None
%
% Returns: None (Starts the Pente game and executes subsequent game-related actions based on user choices.)
%
% Algorithm:
%   1. Displays a welcome message to the user.
%   2. Prompts the user to choose between starting a new game or loading a saved game.
%   3. Obtains and validates the user's choice.
%   4. If the user chooses to start a new game, initiates a tournament.
%   5. If the user chooses to load a saved game, loads the game state and continues the tournament.
%   6. Updates and displays tournament scores.
%   7. Asks the user if they want to continue the tournament.
%   8. If the user wants to continue, the tournament proceeds; otherwise, it ends.
%   9. Declares the winner of the tournament and displays final scores.
%
% Assistance Received: None
% *********************************************************************
game :-
    welcome(Response),
    (
        Response = 2 ->
        (load_game(GameState),
        GameState = [Board,PlayerColor,Player,OpponentColor,OpponentType,PlayerCaptures, OpponentCapture, HumanScore, ComputerScore],

        play_game(Board,PlayerColor,Player,OpponentColor,OpponentType,PlayerCaptures, OpponentCapture, 5,HumanScore, ComputerScore, Result),
        calculate_score(Result, HumanTotal, ComputerTotal),

        HumanFinalScore is HumanScore + HumanTotal,
        ComputerFinalScore is ComputerScore + ComputerTotal,

        format('-----Tournament Scores-----~n'),
        format('Human Scores: ~a~n', [HumanFinalScore]),
        format('Computer Scores: ~a~n', [ComputerFinalScore]),
        format('Continue the tournament? (Enter ''y'' to confirm!): ~n'),
        read_line_to_string(user_input, TournamentResponse),
        (string_lower(TournamentResponse, "y") ->
            write('Tournament continues...'), nl,
            write('---------------------------------'), nl,
            tournament(HumanFinalScore, ComputerFinalScore)
        ;
            format('Tournament ended.~n'),
            declare_winner(HumanFinalScore, ComputerFinalScore)
        )
        )
        ;
        Response = 1 ->
            tournament(0, 0)
        ;
        write('Invalid choice. Try again.'), nl,
        game
    ).

    
