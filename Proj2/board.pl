:-use_module(library(lists)).
:-use_module(library(clpfd)).

solve(L1, L2, L3, L4, L5, L6, L7):-
  L1 = [P1,P2,P2,P3,P3,P3,P3],
  L2 = [P1,P1,P2,P7,P7,P3,P8],
  L3 = [P4,P5,P2,P2,P7,P8,P8],
  L4 = [P4,P5,P5,P2,P9,P9,P8],
  L5 = [P4,P4,P6,P2,P2,P9,P8],
  L6 = [P4,P11,P6,P6,P2,P10,P10],
  L7 = [P11,P11,P11,P11,P2,P2,P1],
  domain(L1,0,1),
  domain(L2,0,1),
  domain(L3,0,1),
  domain(L4,0,1),
  domain(L5,0,1),
  domain(L6,0,1),
  domain(L7,0,1),
  P1 + P2 + P2 + P3 + P3 + P3 + P3 #= 5,
  P1 + P1 + P2 + P7 + P7 + P3 + P8 #= 5,
  P4 + P5 + P5 + P2 + P9 + P9 + P8 #= 4,
  P4 + P11 + P6 + P6 + P2 + P10 + P10 #= 4.
