:- use_module(library(clpfd)).
:- use_module(library(lists)).
:-include('board.pl').
:-include('displays.pl').

solve_easy:-board_1(B),initial_1(Bi),vertical_1(V),horizontal_1(H),show_initial_board(Bi,8,1,V,H),solve(B,V,H,Bf),show_final_board(Bf,V,H,8,1,0,7,49,1).
solve_medium:-board_2(B),initial_2(Bi),vertical_2(V),horizontal_2(H),show_initial_board(Bi,11,1,V,H),solve(B,V,H,Bf),show_final_board(Bf,V,H,11,1,0,10,100,1).
solve_hard:-board_3(B),initial_3(Bi),vertical_3(V),horizontal_3(H),show_initial_board(Bi,16,1,V,H),solve(B,V,H,Bf),show_final_board(Bf,V,H,16,1,0,15,225,1).

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
