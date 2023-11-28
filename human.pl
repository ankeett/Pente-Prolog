:-compile('board.pl').

% Define a predicate to convert a character to its corresponding numerical value
char_to_num(Char, Num) :-
    char_code(Char, Code),
    Num is Code - 65. % Subtracting 65 instead of 64 to start from 0.

% Define the main predicate to convert a move to row and column
% Define the main predicate to convert a move to row and column
convertMove(Move, Row, Col) :-
    writeln('Inside convertMove'),
    atom_chars(Move, [Char|Rest]),
    char_to_num(Char, Col),
    writeln('Char and Col: '), writeln([Char, Col]),
    atomic_list_concat(Rest, RowAtom),
    atom_number(RowAtom, Row1),
    Row is Row1 - 1, % Subtracting 1 to start rows from 0.
    writeln('Converted move: '), writeln([Row, Col]).


human_move(Board, PlayerColor, MoveCount, PlayerMove) :-
    write('Player '), write(PlayerColor), write(' move: '),
    read_line_to_string(user_input, PlayerMoveString),
    atom_string(PlayerMove, PlayerMoveString),
    convertMove(PlayerMove, Row, Col),

    (valid_move(Board, [Row, Col], MoveCount)
    ->  true
    ;   (write('Invalid move'), nl, human_move(Board, PlayerColor, MoveCount, PlayerMove))
    ).
    
    
    

valid_move(Board, [Row, Col], MoveCount) :-
    Row >= 0,
    Row < 19,
    Col >= 0,
    Col < 19.
    



