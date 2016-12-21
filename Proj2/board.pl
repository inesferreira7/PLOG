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

vertical([5,5,-1,4,-1,4,-1]).
horizontal([-1,3,-1,-1,-1,5,-1]).

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
