:-use_module(library(lists)).
:-use_module(library(clpfd)).


board_1([
          [P1,P2,P2,P3,P3,P3,P3],
          [P1,P1,P2,P7,P7,P3,P8],
          [P4,P5,P2,P2,P7,P8,P8],
          [P4,P5,P5,P2,P9,P9,P8],
          [P4,P4,P6,P2,P2,P9,P8],
          [P4,P11,P6,P6,P2,P10,P10],
          [P11,P11,P11,P11,P2,P2,P1]
          ]).

rest([5,5,-1,4,-1,4,-1]).

cenas(B,R,N):-
  element(N,R,S),
  S #= -1,
  N1 is N - 1,
  cenas(B,R,N1).


cenas(B,R,N):-
  element(N, R, S),
  element(N,B,L),
  sum(L,#=,S),
  N1 is N - 1,
  cenas(B,R,N1).

cenas([],[],1).

flatten([],[]).
flatten([LH|LT], Flattened) :-
	is_list(LH),
	flatten(LH, FlattenedTemp),
	append(FlattenedTemp, LT2, Flattened),
	flatten(LT, LT2).

flatten([LH | LT], [LH | FlattenedT]) :-
	\+is_list(LH),
	flatten(LT, FlattenedT).


solve(B,R,X):-
  length(B,X),
  cenas(B,R,X).
