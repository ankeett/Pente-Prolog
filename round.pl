:-compile('board.pl').
:-compile('computer.pl').
:-compile('human.pl').

% *********************************************************************
% start_game/2
%
% Purpose: Initiates a new game, tosses a coin to decide who plays first, and determines the player and opponent types based on the toss result.
%
% Arguments:
%   - PlayerType: The type of the player (human or computer).
%   - OpponentType: The type of the opponent (human or computer).
%
% Returns: None (The PlayerType and OpponentType are instantiated as results.)
%
% Algorithm:
%   1. Displays a message indicating the start of a new game.
%   2. Tosses a coin to decide who plays first.
%   3. Prompts the user to enter 1 for heads or 2 for tails.
%   4. Validates the user input and determines the winner of the toss.
%   5. Assigns the player and opponent types based on the toss result.
%
% Assistance Received: None
% *********************************************************************

start_game(PlayerType, OpponentType) :-
    writeln('Starting a new game'),
    writeln('Tossing a coin to decide who plays first'),
    
    % Define a predicate to toss a coin
    toss_coin(Tossed),

    % Define a predicate to get user input
    get_user_input(UserInput),

    % Define a predicate to check if the user won the toss
    (UserInput =:= Tossed ->
        writeln('You won the toss.'),
        writeln('You will play: White'),
        PlayerType = human,
        OpponentType = computer
    ;
        writeln('Computer won the toss.'),
        writeln('You will play: Black'),
        PlayerType = computer,
        OpponentType = human
    ).

%helper function to get user input 
get_user_input(UserInput) :-
    write('Enter 1 for heads or 2 for tails: '),
    flush_output,
    read_line_to_string(user_input, UserInputString),
    (
        number_string(UserInput, UserInputString),
        (UserInput = 1 ; UserInput = 2) ->
            writeln('You entered:'),
            writeln(UserInput)
        ;
            writeln('Invalid input. Please enter 1 for heads or 2 for tails.'),
            nl,
            get_user_input(UserInput)
    ;
        writeln('Invalid input. Please enter a valid number (1 or 2).'),
        nl,
        get_user_input(UserInput)
    ).




% Define a predicate to toss a coin
toss_coin(Tossed) :-
    random_between(1, 2, Tossed).

% *********************************************************************
% play_game/11
%
% Purpose: Initiates and plays a round of the Pente game, handling the turns and interactions between the human player and computer opponent.
%
% Arguments:
%   - Board: The current state of the game board.
%   - PlayerColor: The color of the current player (human or computer).
%   - PlayerType: The type of the current player (human or computer).
%   - OpponentColor: The color of the opponent (human or computer).
%   - OpponentType: The type of the opponent (human or computer).
%   - PlayerCaptures: The number of captures by the current player.
%   - OpponentCaptures: The number of captures by the opponent.
%   - MoveCount: The current move count.
%   - HumanTournament: The total score of the human player in the current tournament.
%   - ComputerTournament: The total score of the computer player in the current tournament.
%   - Result: The result of the round, including the updated game state and scores.
%
% Returns: None (The Result is instantiated as a result.)
%
% Algorithm:
%   1. Prints the current state of the game board.
%   2. Displays a message indicating the current player's turn.
%   3. Prompts the user to enter a move or call the computer to get a move.
%   4. Validates the user input, makes the move, and checks for a win or capture.
%   5. Handles the recursive calls for the next moves or game termination.
%
% Assistance Received: None
% *********************************************************************

%play the round
play_game(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, MoveCount, HumanTournament, ComputerTournament, Result) :-
    print_2d_board(Board),
    writeln('--------------------------------------------'),
    format('~a\'s turn.~n', [PlayerType]),
    format('~a\'s color: ~a~n', [PlayerType, PlayerColor]),
    writeln('--------------------------------------------'),
    (
        (PlayerType = human ->
            %ask if user wants to quit
            write('Do you want to quit? (Enter y to quit) '),nl,
            flush_output,
            read_line(Response),

            (string_lower(Response,'y') ->
                quitTheGame(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, HumanTournament, ComputerTournament, Result),
                Result = [Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, HumanTournament, ComputerTournament, 0]

                ;
            
                getUserMove(Board,PlayerColor, PlayerType,OpponentColor,OpponentType,PlayerCaptures,OpponentCaptures,MoveCount, HumanTournament, ComputerTournament, PlayerMove),
                PlayerMove = [Row, Col],

                place_stone(Board, Row, Col, PlayerColor, NewBoard),
        
                (check_n(NewBoard, PlayerColor,[ Row, Col],5) ->
                    format('You win!~n'),
                    print_2d_board(NewBoard),
                
                    %calculate score as game ended
                    Result = [NewBoard, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, HumanTournament, ComputerTournament, 5]
                    % calculate_score(Result, _, _)

                    ;

                    (recursively_check_capture(NewBoard,Row,Col,PlayerColor,PlayerCaptures,NewBoard2,NumCaptures)
                    ->
                        captures(human, NumCaptures,computer, OpponentCaptures),

                        ( NumCaptures >=5 ->
                            format('You win!~n'),
                            print_2d_board(NewBoard2),
                            Result = [NewBoard2, PlayerColor, PlayerType, OpponentColor, OpponentType, NumCaptures, OpponentCaptures, HumanTournament, ComputerTournament, 0]
                            % calculate_score(Result, _, _)
                        ;

                            NextMoveCount is MoveCount + 1,
                            play_game(NewBoard2, OpponentColor, OpponentType, PlayerColor, PlayerType, OpponentCaptures, NumCaptures,NextMoveCount, HumanTournament, ComputerTournament,Result)
                        )

                    ;
                        NextMoveCount is MoveCount + 1,
                        captures(human, PlayerCaptures,computer, OpponentCaptures),
                        play_game(NewBoard, OpponentColor, OpponentType, PlayerColor, PlayerType, OpponentCaptures, PlayerCaptures,NextMoveCount, HumanTournament, ComputerTournament, Result)

                    )
                )
            )
        )

        ;

        (PlayerType = computer ->

            %ask if user wants to quit
            write('Do you want to quit? (Enter y to quit) '),nl,
            flush_output,
            read_line(Response),

        
            (string_lower(Response,'y') ->
                quitTheGame(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, HumanTournament, ComputerTournament, Result),
                Result = [Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, HumanTournament, ComputerTournament, 0]
                % calculate_score(Result, HumanScore, ComputerScore)
                ;
                % get the move from the computer
                computer_move(Board, PlayerColor, MoveCount,ComputerMove),
                ComputerMove = [Row, Col, MoveInfo],
                write('Computer\'s move: '), writeln(MoveInfo),

                place_stone(Board, Row, Col, PlayerColor, NewBoard),

                (check_n(NewBoard, PlayerColor,[ Row, Col],5) ->
                    format('Computer wins!~n'),
                    print_2d_board(NewBoard),
                
                    %calculate score as game ended
                    Result = [NewBoard, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures,HumanTournament, ComputerTournament, 5]
                    % calculate_score(Result, _, _)

                    ;

                    (recursively_check_capture(NewBoard,Row,Col,PlayerColor,PlayerCaptures,NewBoard2,NumCaptures)
                    ->
                        captures(computer, NumCaptures, human,  OpponentCaptures),

                        (
                            NumCaptures >= 5 ->
                            format('Computer wins!~n'),
                            print_2d_board(NewBoard2),
                            Result = [NewBoard2, PlayerColor, PlayerType, OpponentColor, OpponentType, NumCaptures, OpponentCaptures, HumanTournament, ComputerTournament, 0]
                            % calculate_score(Result, _, _)
                            ;
                            
                            NextMoveCount is MoveCount + 1,
                            play_game(NewBoard2, OpponentColor, OpponentType, PlayerColor, PlayerType, OpponentCaptures, NumCaptures,NextMoveCount, HumanTournament, ComputerTournament, Result)
                        )



                    ;
                        NextMoveCount is MoveCount + 1,
                        captures(human, PlayerCaptures,computer, OpponentCaptures),
                        play_game(NewBoard, OpponentColor, OpponentType, PlayerColor, PlayerType, OpponentCaptures, PlayerCaptures,NextMoveCount, HumanTournament, ComputerTournament, Result)
                    )

                )
            )
            
        )
    ).

% *********************************************************************
% calculate_score/3
%
% Purpose: Calculates the scores for both the human and computer players based on the result of a game.
%
% Arguments:
%   - Result: The result of the game, including the updated game state and scores.
%   - HumanScore: The final score of the human player.
%   - ComputerScore: The final score of the computer player.
%
% Returns: None (The HumanScore and ComputerScore are instantiated as results.)
%
% Algorithm:
%   1. Extracts relevant information from the Result, such as the board, winner color, winner type, opponent color, winner captures, opponent captures, and whether there is a five-in-a-row.
%   2. Counts the number of captures for both players and additional scores based on the game state.
%   3. Prints the human and computer scores.
%
% Assistance Received: None
% *********************************************************************

calculate_score(Result, HumanScore, ComputerScore) :-
    Result = [Board, WinnerColor, WinnerType, OpponentColor, _, WinnerCapture, OpponentCapture, _, _, IsFiveInARow],
    (   WinnerType = human
        
    ->  count_four(Board,WinnerColor,Count),
        count_four(Board,OpponentColor,Count2),

        HumanScore is WinnerCapture + IsFiveInARow + Count,
        ComputerScore is OpponentCapture + Count2,
        format('Human Scores: ~a~n', [HumanScore]),
        format('Computer Scores: ~a~n', [ComputerScore])

        
    ;   count_four(Board,WinnerColor,Count),
        count_four(Board,OpponentColor,Count2),
    
        HumanScore is OpponentCapture + Count2, 
        ComputerScore is WinnerCapture + IsFiveInARow + Count, 
        format('Human Scores: ~a~n', [HumanScore]),
        format('Computer Scores: ~a~n', [ComputerScore])
        
    ).

% *********************************************************************
% captures/4
%
% Purpose: Displays the captures of both players during a turn.
%
% Arguments:
%   - PlayerType: The type of the player (human or computer) whose captures are displayed.
%   - PlayerCapture: The number of captures made by the player.
%   - OpponentType: The type of the opponent player (human or computer).
%   - OpponentCapture: The number of captures made by the opponent player.
%
% Returns: None (Displays the captures for both players.)
%
% Algorithm:
%   1. Formats and prints the captures of both the player and the opponent.
%
% Assistance Received: None
% *********************************************************************
captures(PlayerType, PlayerCapture, OpponentType, OpponentCapture) :-
    format('--------------------------------------------~n'),
    format('~w Captures: ~w~n', [PlayerType, PlayerCapture]),
    format('~w Captures: ~w~n', [OpponentType, OpponentCapture]),
    format('--------------------------------------------~n').



% *********************************************************************
% start_round/5
%
% Purpose: Initiates a new round of the game with the specified player types and scores.
%
% Arguments:
%   - PlayerType: The type of the player (human or computer) who plays first.
%   - OpponentType: The type of the opponent player (human or computer).
%   - HumanScore: The initial score of the human player.
%   - ComputerScore: The initial score of the computer player.
%   - Result: The result of the round, including the final board state and scores.
%
% Returns: None (Initiates and plays a new round of the game.)
%
% Algorithm:
%   1. Creates a 2D board using make_2d_board/1.
%   2. Calls play_game/11 to start playing the game.
%   3. Displays "Game Over" at the end of the game.
%
% Assistance Received: None
% *********************************************************************
start_round(PlayerType, OpponentType, HumanScore, ComputerScore,Result) :-
    make_2d_board(Board),
    play_game(Board, w, PlayerType, b, OpponentType,0,0, 1, HumanScore, ComputerScore, Result),
    writeln('Game Over.').

