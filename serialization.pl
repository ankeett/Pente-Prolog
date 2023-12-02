:-compile('board.pl').

deserialize(GameState) :-
    write('Enter the name of the file to load the game from: '),
    read_line_to_string(user_input, GameStateFile),
    open(GameStateFile, read, Stream),
    read(Stream, GameState),
    close(Stream).

load_game( Result) :-
    deserialize( GameState),
    GameState = [Board, HumanCaptures, HumanScore, ComputerCaptures,ComputerScore,PlayerType,NextColor],

    (PlayerType = human
        ->  OpponentType = 'Computer',
            Player = 'Human'
        ;   Player = 'Computer',
            OpponentType = 'Human'
    )
    ,
    (NextColor = black
        ->  PlayerColor = 'B'
        ;   PlayerColor = 'W'
    ),

    (PlayerColor = 'B'
        ->  OpponentColor = 'W'
        ;   OpponentColor = 'B'
    ),

    Result = [Board,PlayerColor,Player,OpponentColor,OpponentType,HumanCaptures, ComputerCaptures, HumanScore, ComputerScore].





    


