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

board_3([
        [P1 ,P1 ,P2 ,P2 ,P3 ,P3 ,P3 ,P3 ,P3 ,P4 ,P4 ,P5 ,P5 ,P5 ,P5 ],
        [P1 ,P2 ,P2 ,P6 ,P6 ,P3 ,P7 ,P7 ,P15,P4 ,P4 ,P4 ,P5 ,P5 ,P5 ],
        [P1 ,P9 ,P9 ,P6 ,P6 ,P8 ,P8 ,P7 ,P15,P15,P16,P4 ,P4 ,P17,P17],
        [P1 ,P10,P9 ,P8 ,P8 ,P8 ,P7 ,P7 ,P15,P15,P16,P16,P16,P17,P17],
        [P10,P10,P9 ,P9 ,P21,P22,P23,P23,P23,P23,P16,P16,P18,P18,P17],
        [P10,P10,P21,P21,P21,P22,P23,P20,P20,P20,P20,P20,P18,P18,P18],
        [P10,P10,P14,P24,P22,P22,P22,P26,P26,P34,P19,P19,P19,P19,P18],
        [P11,P13,P14,P24,P24,P25,P26,P26,P34,P34,P37,P37,P19,P38,P38],
        [P11,P13,P14,P14,P14,P25,P26,P26,P34,P37,P37,P38,P38,P38,P39],
        [P11,P13,P13,P13,P27,P25,P25,P26,P34,P34,P37,P37,P37,P38,P39],
        [P11,P12,P28,P27,P27,P27,P27,P26,P34,P35,P41,P41,P41,P41,P40],
        [P11,P12,P28,P27,P29,P29,P29,P35,P35,P35,P41,P40,P40,P40,P40],
        [P12,P12,P28,P28,P29,P30,P30,P36,P36,P36,P36,P42,P42,P42,P42],
        [P12,P12,P28,P28,P30,P30,P30,P30,P31,P31,P31,P31,P31,P33,P33],
        [P12,P12,P28,P18,P43,P43,P43,P30,P31,P32,P32,P32,P31,P33,P33]
        ]).

vertical([5,5,-1,4,-1,4,-1]).
horizontal([-1,3,-1,-1,-1,5,-1]).

vertical3([4,8,10,6,4,6,9,6,7,6,5,12,7,4,10]).
horizontal3([9,2,11,7,7,8,6,6,7,10,6,6,4,6,9]).

fill_board(_,_,0).

fill_board(Board,Vertical,N):-
  element(N,Vertical,LineSum),
  LineSum #= -1,
  N1 is N - 1,
  fill_board(Board,Vertical, N1).


fill_board(Board,Vertical,N):-
  select_list(Board, 1, N, List),
  element(N, Vertical, LineSum),
  domain(List,0,1),
  sum(List,#=,LineSum),
  N1 is N - 1,
  fill_board(Board,Vertical,N1).

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

get_col([Head|Tail], N, List):-
    element(N, Head, Val),
    append(List, [Val], NewList),
    get_col(Tail,N,NewList).

get_col([],_,_).

flatten([],[]).
flatten([LH|LT], Flattened) :-
	is_list(LH),
	flatten(LH, FlattenedTemp),
	append(FlattenedTemp, LT2, Flattened),
	flatten(LT, LT2).

flatten([LH | LT], [LH | FlattenedT]) :-
	\+is_list(LH),
	flatten(LT, FlattenedT).

solve(Board,Vertical,Horizontal,Length):-
  length(Board,Length),
  fill_board(Board,Vertical,Length),
  board_vertical(Board, Horizontal,Length),
  flatten(Board,BoardFlat),
  labeling([],BoardFlat),
  write(BoardFlat).
