:-compile('board.pl').
:-compile('computer.pl').
:-compile('human.pl').






start_game(PlayerType, OpponentType) :-
    writeln('Starting a new game'),
    writeln('Tossing a coin to decide who plays first'),
    
    % Define a predicate to toss a coin
    toss_coin(Tossed),
    writeln(Tossed),

    % Define a predicate to get user input
    get_user_input(UserInput),
    writeln(UserInput),

    % Define a predicate to check if the user won the toss

    
    (UserInput =:= Tossed ->
        writeln('You won the toss.'),
        writeln('You will play: White'),
        PlayerType = 'Human',
        OpponentType = 'Computer'
    ;
        writeln('Computer won the toss.'),
        writeln('You will play: Black'),
        PlayerType = 'Computer',
        OpponentType = 'Human'
    ).

% Define a predicate to get user input
get_user_input(UserInput) :-
    write('Enter 1 for heads or 2 for tails: '),
    flush_output,
    read_line_to_string(user_input, UserInputString),
    (catch(number_string(UserInput, UserInputString), _, fail) ->
        writeln('You entered:'),
        writeln(UserInput)
    ;
        writeln('Invalid input. Please enter 1 for heads or 2 for tails.'),
        nl,
        get_user_input(UserInput)
    ).


% Define a predicate to toss a coin
toss_coin(Tossed) :-
    random_between(1, 2, Tossed).


%play the round
% play_game(Board, _, _, _, _, MoveCount, 'WinConditionMet') :-
   
%     writeln('Win condition met. Game over.'),
%     print_2d_board(Board),
%     true.
    % Add your win condition check here
    % For example, if a certain player has won, you can check it here.

play_game(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, MoveCount, Result) :-
    print_2d_board(Board),
    writeln('--------------------------------------------'),
    format('~a\'s turn.~n', [PlayerType]),
    format('~a\'s color: ~a~n', [PlayerType, PlayerColor]),
    writeln('--------------------------------------------'),
    (
        (PlayerType = 'Human' ->
            % get the move from the user
            human_move(Board, PlayerColor, MoveCount, PlayerMove),

            convertMove(PlayerMove, Row, Col),
            %check for five in a row
            place_stone(Board, Row, Col, PlayerColor, NewBoard),
    
            (check_five(NewBoard, PlayerColor,[ Row, Col]) ->
                format('You win!~n'),
                print_2d_board(NewBoard),
            
                %calculate score as game ended
                Result = [NewBoard, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, 5],

                % Result = [Board, WinnerColor, WinnerType, OpponentColor, OpponentType, WinnerCapture, OpponentCapture, IsFiveInARow],

                calculate_score(Result, HumanScore, ComputerScore)

                ;
            (recursively_check_capture(NewBoard,Row,Col,PlayerColor,PlayerCaptures,NewBoard2,NumCaptures)
            ->
                print_2d_board(NewBoard2),
                NextMoveCount is MoveCount + 1,

                play_game(NewBoard2, OpponentColor, OpponentType, PlayerColor, PlayerType, OpponentCaptures, PlayerCaptures,NextMoveCount, Result)

            ;
                NextMoveCount is MoveCount + 1,

                play_game(NewBoard, OpponentColor, OpponentType, PlayerColor, PlayerType, OpponentCaptures, PlayerCaptures,NextMoveCount, Result)

            ),



            NextMoveCount is MoveCount + 1,

            play_game(NewBoard, OpponentColor, OpponentType, PlayerColor, PlayerType, OpponentCaptures, PlayerCaptures,NextMoveCount, Result)



            )


            
            
        )

        ;

        (PlayerType = 'Computer' ->
            % get the move from the computer
            computer_move(Board, PlayerColor, MoveCount,ComputerMove),
            writeln('Computer\'s move: '),
            ComputerMove = [Row, Col, MoveInfo],
            place_stone(Board, Row, Col, PlayerColor, NewBoard),
            print_2d_board(NewBoard),

            (check_five(NewBoard, PlayerColor,[ Row, Col]) ->
                format('You win!~n'),
                print_2d_board(NewBoard),
            
                %calculate score as game ended
                Result = [NewBoard, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, 4]


                ;

                write('no win')

            ),   

            (recursively_check_capture(NewBoard,Row,Col,PlayerColor,0,NewBoard2,NumCaptures)
            ->
                print_2d_board(NewBoard2),
                NextMoveCount is MoveCount + 1,

                play_game(NewBoard2, OpponentColor, OpponentType, PlayerColor, PlayerType, OpponentCaptures, PlayerCaptures,NextMoveCount, Result)

            ;
                NextMoveCount is MoveCount + 1,

                play_game(NewBoard, OpponentColor, OpponentType, PlayerColor, PlayerType, OpponentCaptures, PlayerCaptures,NextMoveCount, Result)
            )

        )
    ).

    
    calculate_score(Result, HumanScore, ComputerScore) :-
        Result = [Board, WinnerColor, WinnerType, OpponentColor, OpponentType, WinnerCapture, OpponentCapture, IsFiveInARow],
        (   WinnerType = 'Human'
        ->  HumanScore is WinnerCapture + IsFiveInARow ,
        ComputerScore is OpponentCapture,
            format('Human Scores: ~a~n', [HumanScore]),
            format('Computer Scores: ~a~n', [ComputerScore])

            
        ;   HumanScore is OpponentCapture, 
        ComputerScore is WinnerCapture + IsFiveInARow, 
            format('Human Scores: ~a~n', [HumanScore]),
            format('Computer Scores: ~a~n', [ComputerScore])
            
        ).

% Start the game
start_game :-
    make_2d_board(Board),
    start_game(PlayerType, OpponentType),
    play_game(Board, 'W', PlayerType, 'B', OpponentType,0,0, 1, Result),
    writeln('Game Over. Result: '),
    writeln(Result).
