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
        [P1,P1,P1,P1,P2,P2,P2,P3,P3,P4],
        [P5,P5,P1,P1,P2,P2,P2,P3,P3,P4],
        [P6,P7,P7,P8,P8,P8,P3,P3,P3,P4],
        [P6,P7,P9,P9,P9,P8,P8,P10,P11,P4],
        [P7,P7,P9,P9,P12,P12,P8,P10,P11,P13],
        [P7,P7,P16,P16,P12,P14,P15,P15,P13,P13],
        [P15,P15,P16,P16,P16,P14,P17,P17,P17,P17],
        [P15,P15,P16,P18,P14,P14,P19,P17,P17,P17],
        [P20,P20,P16,P18,P14,P14,P19,P21,P21,P22],
        [P20,P20,P20,P18,P23,P23,P23,P23,P23,P22]
        ]).



vertical_1([5,5,-1,4,-1,4,-1]).
horizontal_1([-1,3,-1,-1,-1,5,-1]).

vertical_2([-1,-1,-1,4,4,3,-1,-1,-1,8]).
horizontal_2([7,-1,-1,5,-1,-1,5,6,-1,-1]).


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

solve(Board,Vertical,Horizontal):-
  length(Board,Length),
  fill_board(Board,Vertical,Length),
  board_vertical(Board,Horizontal,Length),
  flatten(Board,BoardFlat),
  labeling([],BoardFlat),
  write(BoardFlat).
