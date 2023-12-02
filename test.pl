:-compile('board.pl').
:-compile('computer.pl').
:-compile('human.pl').

my_test:-
    make_2d_board(Board),

    place_stone(Board,5,5,'W',NewBoard),
    place_stone(NewBoard,4,5,'W',NewBoard1),
    place_stone(NewBoard1,3,5,'W',NewBoard2),

    print_2d_board(NewBoard2),

    %test case for makeNMove
    makeNMove(NewBoard2,'W',4,Result),
    write(Result).

    