:-compile('board.pl').
:-compile('computer.pl').
:-compile('human.pl').

my_test:-
    make_2d_board(Board),
    place_stone(Board,5,5,'W',NewBoard),
    place_stone(NewBoard,5,6,'B',NewBoard2),
    place_stone(NewBoard2,5,7,'B',NewBoard3),

    place_stone(NewBoard3,5,9,'W',NewBoard5),
    place_stone(NewBoard5,6,8,'B',NewBoard6),
    place_stone(NewBoard6,7,8,'B',NewBoard7),
    place_stone(NewBoard7,8,8,'W',NewBoard8),

    place_stone(NewBoard8,4,5,'B',NewBoard9),
    place_stone(NewBoard9,3,5,'B',NewBoard10),
    %  findWinningMove(NewBoard5, 'W', Result),
    %     Result = [X,Y],
    
    %     write(X),
    %     write(Y),nl,


    % check_five(NewBoard4,'W',[0,1]),
    

    print_2d_board(NewBoard10),

    % checkCell(NewBoard4,5,4,'W',Result1),
    % write(Result1),

    defendCapturePosition(NewBoard10, 'B', Result3),
    % checkCellCapture(NewBoard5, 0,0,'W',0,MaxRow,MaxCol, Result).
    Result3 = [X,Y],
    write(X),
    write(Y),nl.

    



    % checkNextCell(NewBoard4,5,4,'W',Result2),
    % defendWinningMove(NewBoard5, 'B', Result2),
    %  Result2 = [X,Y],


    % write(X),
    % write(Y),nl.



    % (check_five(NewBoard5,'W',[5,5])

    % ->
    % write('win')
    % ;
    % write('')
    
    % ).
    

    

    % print_2d_board(NewBoard7),

    %test check_capture_direction, print captured if true
    % capture_pair(NewBoard4,5,5,1,1,'W','B',2,NewBoard5),
    % check_capture(NewBoard7,0,0,'W',NewBoard8).
    % print_2d_board(NewBoard8).

    % (recursively_check_capture(NewBoard5,5,8,'W',0,NewBoard8,Result)
    % ->
    %     print_2d_board(NewBoard8),
    %     write(Result)

        
    % ;
    % write('no capture')
    % ),

    % (
    %     NewBoard8 = NewBoard5
    % ->
    %     write('no capture')
    % ;
    %     write('capture')
    % ).





    % check_capture_direction(NewBoard4,5,5,1,0,'W','B',2),  
    % check_capture_direction(NewBoard4,5,5,0,-1,'W','B',2),
    % check_capture_direction(NewBoard4,5,5,0,1,'W','B',2),
    % check_capture_direction(NewBoard4,5,5,-1,0,'W','B',2),
    % check_capture_direction(NewBoard4,5,5,1,1,'W','B',2),
    % check_capture_direction(NewBoard4,5,5,-1,-1,'W','B',2),
    % check_capture_direction(NewBoard4,5,5,-1,1,'W','B',2),
    % check_capture_direction(NewBoard4,5,5,1,-1,'W','B',2).

    


    % check_capture(NewBoard4,5,5,'W',NewBoard5).    
    