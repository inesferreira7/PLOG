:-use_module(library(lists)).
:-use_module(library(clpfd)).


board_1([
          [P1,P2,P2,P3,P3,P3,P3],
          [P1,P1,P2,P7,P7,P3,P8],
          [P4,P5,P2,P2,P7,P8,P8],
          [P4,P5,P5,P2,P9,P9,P8],
          [P4,P4,P6,P2,P2,P9,P8],
          [P4,P11,P6,P6,P2,P10,P10],
          [P11,P11,P11,P11,P2,P2,P10]
          ]).


board_2([
        [P1,P1,P2,P2,P2,P3,P3,P4,P4,P4],
        [P1,P1,P6,P2,P8,P8,P3,P4,P9,P4],
        [P5,P6,P6,P7,P8,P3,P3,P9,P9,P9],
        [P5,P10,P7,P7,P8,P8,P14,P14,P9,P16],
        [P10,P10,P11,P12,P13,P13,P14,P15,P15,P16],
        [P10,P11,P11,P12,P13,P13,P14,P15,P16,P16],
        [P10,P17,P12,P12,P20,P20,P22,P22,P16,P24],
        [P17,P17,P17,P19,P19,P20,P22,P23,P23,P24],
        [P18,P17,P18,P19,P20,P20,P21,P23,P25,P25],
        [P18,P18,P18,P19,P19,P21,P21,P21,P25,P25]
        ]).

board_3([
        [P1,P1,P2,P2,P3,P3,P3,P3,P3,P4,P4,P5,P5,P5,P5],
        [P1,P2,P2,P6,P6,P7,P8,P8,P9,P4,P4,P4,P5,P5,P5],
        [P1,P12,P12,P6,P6,P7,P7,P8,P9,P9,P10,P4,P4,P4,P4],
        [P1,P13,P12,P7,P7,P7,P8,P8,P9,P9,P10,P10,P10,P4,P4],
        [P13,P13,P12,P12,P14,P15,P16,P16,P16,P16,P10,P10,P11,P11,P4],
        [P13,P13,P14,P14,P14,P15,P16,P18,P18,P18,P18,P18,P11,P11,P11],
        [P13,P13,P25,P24,P15,P15,P15,P17,P17,P38,P20,P20,P20,P20,P21],
        [P27,P26,P25,P24,P24,P15,P17,P17,P38,P38,P22,P22,P20,P21,P21],
        [P27,P26,P25,P25,P25,P15,P17,P17,P38,P22,P22,P21,P21,P21,P23],
        [P27,P26,P26,P26,P30,P15,P15,P17,P38,P38,P22,P22,P22,P21,P23],
        [P27,P28,P29,P30,P30,P30,P30,P17,P38,P39,P22,P22,P22,P22,P23],
        [P27,P28,P29,P30,P31,P31,P31,P39,P39,P39,P22,P23,P23,P23,P23],
        [P28,P28,P29,P29,P31,P32,P32,P40,P40,P40,P40,P36,P36,P37,P37],
        [P28,P28,P29,P29,P32,P32,P32,P32,P34,P35,P35,P35,P35,P42,P42],
        [P28,P28,P29,P29,P33,P33,P33,P32,P34,P41,P41,P41,P35,P42,P42]
        ]).



vertical_1([5,5,-1,4,-1,4,-1]).
horizontal_1([-1,3,-1,-1,-1,5,-1]).

vertical_2([5,5,6,6,-1,5,5,-1,5,5]).
horizontal_2([6,6,-1,-1,-1,-1,-1,-1,4,4]).

vertical_3([9,8,10,6,4,6,9,6,6,6,4,12,7,4,10]).
horizontal_3([-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]).





board_horizontal(_,_,0).

board_horizontal(Board,Vertical,N):-
  element(N,Vertical,LineSum),
  LineSum #= -1,
  N1 is N - 1,
  board_horizontal(Board,Vertical, N1).


board_horizontal(Board,Vertical,N):-
  select_list(Board, 1, N, List),
  element(N, Vertical, LineSum),
  domain(List,0,1),
  sum(List,#=,LineSum),
  N1 is N - 1,
  board_horizontal(Board,Vertical,N1).

select_list([Head|Tail], N1, N, List):-
  N1 #\= N,
  N2 is N1 + 1,
  select_list(Tail, N2, N, List).

select_list([Head|_], N, N, Head).

select_list([],_,_,[]).

board_vertical(_,_,0).

board_vertical(Board, Horizontal, N):-
  element(N, Horizontal, ColSum),
  ColSum #= -1,
  N1 is N - 1,
  board_vertical(Board, Horizontal, N1).


board_vertical(Board, Horizontal, N):-
  get_col(Board, N, Col),
  element(N, Horizontal, ColSum),
  domain(Col, 0, 1),
  sum(Col, #=, ColSum),
  N1 is N - 1,
  board_vertical(Board, Horizontal, N1).


  get_col([],_,[]).
  get_col([Head|Tail], N, [ColHead|ColTail]):-
    element(N, Head, Val),
    ColHead = Val,
    get_col(Tail,N,ColTail).



flatten([],[]).
flatten([LH|LT], Flattened) :-
	is_list(LH),
	flatten(LH, FlattenedTemp),
	append(FlattenedTemp, LT2, Flattened),
	flatten(LT, LT2).

flatten([LH | LT], [LH | FlattenedT]) :-
	\+is_list(LH),
	flatten(LT, FlattenedT).

  reset_timer :- statistics(walltime,_).
  print_time :-
  	statistics(walltime,[_,T]),
  	TS is ((T//10)*10)/1000,
  	nl, write('Time: '), write(TS), write('s'), nl, nl.


solve(Board,Vertical,Horizontal,BoardFlat):-
  length(Board,Length),
  board_vertical(Board,Horizontal,Length),
  board_horizontal(Board,Vertical,Length),
  flatten(Board,BoardFlat),
  reset_timer,
  labeling([],BoardFlat),
  print_time,
  fd_statistics.
