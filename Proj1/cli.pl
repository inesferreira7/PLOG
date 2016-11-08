%%%%%%%%%%%%%% Clears screen screen %%%%%%%%%%%%%%%%%
clearScreen :-
        printBlank(65).

%%%%%%%%%%%%%% Blank %%%%%%%%%%%%%%%%
printBlank(X) :-
        X > 0,
        nl,
        X1 is X-1,
        printBlank(X1).

printBlank(_).

%%%%%%%%%%%% Interface - menu %%%%%%%%%%%%%%%

mainMenu :-
        clearScreen,
        printMainMenu,
        get_char(In),
        (
          In = '1' -> write('Play game'), play_game(_X,_Y,_Z,_S);
          In = '2' -> write('Instructions');
          In = '3' -> write('Exit');

          mainMenu

        ).

%%%%%%%%%%%% Print of main menu %%%%%%%%%%%%

printMainMenu:-
        write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n'),
        write('%%         Martian Chess         %%\n'),
        write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n'),
        write('%%                               %%\n'),
        write('%%           1 - Play            %%\n'),
        write('%%         2 - Instrucoes        %%\n'),
        write('%%           3 - Exit            %%\n'),
        write('%%                               %%\n'),
        write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n').
