:-compile('tournament.pl').

welcome(Result) :-
    write('Welcome to Pente!'), nl,
    write('You will be playing against the computer.'), nl,
    write('Choose:'), nl,
    write('1. Start a new game'), nl,
    write('2. Load a saved game'), nl,
    get_welcome_input.

get_welcome_input(Result) :-
    write('Enter your choice (1 or 2): '),
    read(Choice),
    (
        Choice = 1 -> Result = 1;
        Choice = 2 -> Result = 2;
        write('Invalid choice. Try again.'), nl,
        get_welcome_input
    ).

    main(Response) :-
        (Response \= "1" ->
            (load_game(GameState),
            GameState = [Board,PlayerColor,Player,OpponentColor,OpponentType,HumanCaptures, ComputerCaptures, HumanScore, ComputerScore],
            play_game(Board,PlayerColor,Player,OpponentColor,OpponentType,HumanCaptures, ComputerCaptures, 5,HumanScore, ComputerScore, Result),
            calculate_score(Result, Scores),
            format('-----Tournament Scores-----~n'),
            format('Human Scores: ~a~n', [HumanScore]),
            format('Computer Scores: ~a~n', [ComputerScore]),
            format('Continue the tournament? (Enter ''y'' to confirm!): ~n'),
            read_line_to_string(user_input, TournamentResponse),
            (string_lower(TournamentResponse, "y") ->
                tournament(HumanScore, ComputerScore)
            ;
                format('Tournament ended.~n'),
                declare_winner(HumanScore, ComputerScore)
            ))
        ;
            tournament(0, 0)
        ).









