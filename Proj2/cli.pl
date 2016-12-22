:- use_module(library(clpfd)).
:- use_module(library(lists)).
:-include('board.pl').

solve_easy:-board_1(B),vertical_1(V),horizontal_1(H),solve(B,V,H).
solve_medium:-board_2(B),vertical_2(V),horizontal_2(H),solve(B,V,H).
solve_hard:-board_3(B),vertical_3(V),horizontal_3(H),solve(B,V,H).

clearScreen:-
        printBlank(65).

printBlank(X):-
        X > 0,
        nl,
        X1 is X-1,
        printBlank(X1).

printBlank(_).

mainMenu:-
        clearScreen,
        printMainMenu,
        get_char(In),
        (
          In = '1' -> solve_easy;
          In = '2' -> solve_medium;
          In = '3' -> solve_hard;
          In = '4';
          mainMenu

        ).

printMainMenu:-
        write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n'),
        write('%%         Crazy Pavement        %%\n'),
        write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n'),
        write('%%    Choose board dimensions    %%\n'),
        write('%%            1 - 7x7            %%\n'),
        write('%%           2 - 10x10           %%\n'),
        write('%%           3 - 15x15           %%\n'),
        write('%%                               %%\n'),
        write('%%           4 - Exit            %%\n'),
        write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n').

start_game:-mainMenu.
