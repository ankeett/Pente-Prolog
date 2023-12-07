:-compile('board.pl').

% *********************************************************************
% deserialize/1
%
% Purpose: Deserializes the game state from a file.
%
% Arguments:
%   - GameState: The variable to store the deserialized game state.
%
% Returns: None (Reads the game state from a file and stores it in GameState.)
%
% Algorithm:
%   1. Prompts the user to enter the name of the file to load the game from.
%   2. Reads the game state from the specified file using read/2.
%   3. Closes the file stream after reading.
%
% Assistance Received: None
% *********************************************************************
deserialize(GameState) :-
    write('Enter the name of the file to load the game from: '),
    read_line_to_string(user_input, GameStateFile),
    open(GameStateFile, read, Stream),
    read(Stream, GameState),
    close(Stream).


% *********************************************************************
% load_game/1
%
% Purpose: Loads the game state from a file.
%
% Arguments:
%   - Result: The variable to store the loaded game state.
%
% Returns: None (Reads the game state from a file and stores it in Result.)
%
% Algorithm:
%   1. Calls the deserialize/1 predicate to prompt the user for the file name and read the game state.
%   2. Extracts relevant information from the deserialized game state and structures it as Result.
%
% Assistance Received: None
% *********************************************************************

load_game(Result) :-
    deserialize(GameState),
    GameState = [Board, HumanCaptures, HumanScore, ComputerCaptures,ComputerScore,PlayerType,NextColor],

    (PlayerType = human
        ->  OpponentType = computer,
            PlayerCapture = HumanCaptures,
            OpponentCapture = ComputerCaptures
        ;  
            OpponentType =  human,
            PlayerCapture = ComputerCaptures,
            OpponentCapture = HumanCaptures
    )
    ,
    (NextColor = black
        ->  PlayerColor = b
        ;   PlayerColor = w
    ),

    (PlayerColor = b
        ->  OpponentColor = w
        ;   OpponentColor = b
    ),

    Result = [Board,PlayerColor,PlayerType,OpponentColor,OpponentType,PlayerCapture, OpponentCapture, HumanScore, ComputerScore].

% *********************************************************************
% quitTheGame/10
%
% Purpose: Prompts the user if they want to save the game and saves it if requested.
%
% Arguments:
%   - Board: The current game board.
%   - PlayerColor: The color of the current player.
%   - PlayerType: The type of the current player (human or computer).
%   - OpponentColor: The color of the opponent player.
%   - OpponentType: The type of the opponent player (human or computer).
%   - PlayerCaptures: The number of captures by the current player.
%   - OpponentCaptures: The number of captures by the opponent player.
%   - HumanScore: The current score of the human player.
%   - ComputerScore: The current score of the computer player.
%   - _: A placeholder for the Result variable, which is not used in this predicate.
%
% Returns: None (Prompts the user and saves the game if requested.)
%
% Algorithm:
%   1. Asks the user if they want to save the game.
%   2. If the user enters 'y', prompts for a file name and calls saveGame/9 to save the game state.
%   3. If the user enters 'n' or anything else, informs them that the game is not saved.
%
% Assistance Received: None
% *********************************************************************
quitTheGame(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, HumanScore, ComputerScore, _):-
    write('Do you want to save the game? (y/n): '),
    read_line_to_string(user_input, UserInput),
    (   string_lower(UserInput, 'y')
    ->  write('Enter the name of the file: '),
        read_line_to_string(user_input, FileName),
        saveGame(Board, PlayerColor, PlayerType, OpponentColor, OpponentType, PlayerCaptures, OpponentCaptures, HumanScore, ComputerScore, FileName)
    ;   
        write('Game not saved.~n'),nl
    ).

% *********************************************************************
% saveGame/9
%
% Purpose: Saves the current game state to a file.
%
% Arguments:
%   - Board: The current game board.
%   - PlayerColor: The color of the current player.
%   - PlayerType: The type of the current player (human or computer).
%   - OpponentColor: The color of the opponent player.
%   - OpponentType: The type of the opponent player (human or computer).
%   - PlayerCaptures: The number of captures by the current player.
%   - OpponentCaptures: The number of captures by the opponent player.
%   - HumanScore: The current score of the human player.
%   - ComputerScore: The current score of the computer player.
%   - FileName: The name of the file to save the game state to.
%
% Returns: None (Saves the game state to the specified file.)
%
% Algorithm:
%   1. Opens the specified file in write mode.
%   2. Determines the opponent type and captures based on the player type.
%   3. Converts the player color to a corresponding term (black or white).
%   4. Writes the game state data to the file as a list.
%   5. Closes the file.
%
% Assistance Received: None
% *********************************************************************
saveGame(Board, PlayerColor, PlayerType, _, _, PlayerCaptures, OpponentCaptures, HumanScore, ComputerScore, FileName):-
    open(FileName, write, Stream),

    (
        PlayerType = human
        ->  HumanCaptures = PlayerCaptures,
            ComputerCaptures = OpponentCaptures
            

        ;   HumanCaptures = OpponentCaptures,
            ComputerCaptures = PlayerCaptures
            
    ),

    (
        PlayerColor = b
        ->  Color = black
        ;   Color = white
    ),


    DataList = [Board, 
                    HumanCaptures, HumanScore,
                    ComputerCaptures , ComputerScore, 
                
                PlayerType ,Color],

    write(Stream, DataList),
    close(Stream).