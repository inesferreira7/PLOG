:-use_module(library(lists)).
:-use_module(library(clpfd)).
:-include('board.pl').


initial_1([
          [p1 ,p2 ,p2 ,p3 ,p3 ,p3 ,p3 ],
          [p1 ,p1 ,p2 ,p7 ,p7 ,p3 ,p8 ],
          [p4 ,p5 ,p2 ,p2 ,p7 ,p8 ,p8 ],
          [p4 ,p5 ,p5 ,p2 ,p9 ,p9 ,p8 ],
          [p4 ,p4 ,p6 ,p2 ,p2 ,p9 ,p8 ],
          [p4 ,p11,p6 ,p6 ,p2 ,p10,p10],
          [p11,p11,p11,p11,p2 ,p2 ,p10]
          ]).


initial_2([
          [p1,p1,p2,p2,p2,p3,p3,p4,p4,p4],
          [p1,p1,p6,p2,p8,p8,p3,p4,p9,p4],
          [p5,p6,p6,p7,p8,p3,p3,p9,p9,p9],
          [p5,p10,p7,p7,p8,p8,p14,p14,p9,p16],
          [p10,p10,p11,p12,p13,p13,p14,p15,p15,p16],
          [p10,p11,p11,p12,p13,p13,p14,p15,p16,p16],
          [p10,p17,p12,p12,p20,p20,p22,p22,p16,p24],
          [p17,p17,p17,p19,p19,p20,p22,p23,p23,p24],
          [p18,p17,p18,p19,p20,p20,p21,p23,p25,p25],
          [p18,p18,p18,p19,p19,p21,p21,p21,p25,p25]
          ]).


initial_3([
        [p1,p1,p2,p2,p3,p3,p3,p3,p3,p4,p4,p5,p5,p5,p5],
        [p1,p2,p2,p6,p6,p7,p8,p8,p9,p4,p4,p4,p5,p5,p5],
        [p1,p12,p12,p6,p6,p7,p7,p8,p9,p9,p10,p4,p4,p4,p4],
        [p1,p13,p12,p7,p7,p7,p8,p8,p9,p9,p10,p10,p10,p4,p4],
        [p13,p13,p12,p12,p14,p15,p16,p16,p16,p16,p10,p10,p11,p11,p4],
        [p13,p13,p14,p14,p14,p15,p16,p18,p18,p18,p18,p18,p11,p11,p11],
        [p13,p13,p25,p24,p15,p15,p15,p17,p17,p38,p20,p20,p20,p20,p21],
        [p27,p26,p25,p24,p24,p15,p17,p17,p38,p38,p22,p22,p20,p21,p21],
        [p27,p26,p25,p25,p25,p15,p17,p17,p38,p22,p22,p21,p21,p21,p23],
        [p27,p26,p26,p26,p30,p15,p15,p17,p38,p38,p22,p22,p22,p21,p23],
        [p27,p28,p29,p30,p30,p30,p30,p17,p38,p39,p22,p22,p22,p22,p23],
        [p27,p28,p29,p30,p31,p31,p31,p39,p39,p39,p22,p23,p23,p23,p23],
        [p28,p28,p29,p29,p31,p32,p32,p40,p40,p40,p40,p36,p36,p37,p37],
        [p28,p28,p29,p29,p32,p32,p32,p32,p34,p35,p35,p35,p35,p42,p42],
        [p28,p28,p29,p29,p33,p33,p33,p32,p34,p41,p41,p41,p35,p42,p42]
        ]).

display_horizontal(H,N,N1):-
  N#>=N1,
  element(N1,H,Number),
  (
  Number = -1 -> write('     ');
  write(Number), write('    ')
  ),
  N2 is N1 +1,
  display_horizontal(H,N,N2).

display_horizontal(H,N,N1):-
  nl.

display_horizontal([],_,_).



display_horizontal_final(H,N,N1):-
  N#>=N1,
  element(N1,H,Number),
  (
  Number = -1 -> write('    ');
  write(Number), write('   ')
  ),
  N2 is N1 +1,
  display_horizontal_final(H,N,N2).

display_horizontal_final(H,N,N1):-
  nl.

display_horizontal_final([],_,_).



display_initial_board(Board,N,N1,V):-
  N#>=N1,
  element(N1,V,Number),
  (
  Number = -1 -> write(' '), write(' | ');
  Number >= 10 ->write(Number), write('| ');
  write(Number),write(' | ')
  ),
  select_list(Board,1,N1,Row),
  display_line(Row),
  nl,
  N2 is N1+1,
  display_initial_board(Board,N,N2,V).

display_initial_board(Board,N,N1,V):-
  nl.
display_initial_board([],_,_,[]).


display_line([E1|ES]) :- translate(E1,V), write(V), write(' | '), display_line(ES).

display_line([]).


display_line2([E1|ES]) :-  write(E1), write(' | '), display_line2(ES).

display_line2([]).

display_final_board(Board,N,N1,V,Total,Counter,Lstart,Llen):-
  Total#>=Counter,
  element(N1,V,Number),
  (
  Number = -1 -> write(' '), write(' | ');
  Number >= 10 ->write(Number), write('| ');
  write(Number),write(' | ')
  ),
  sublist(Board,Row,Lstart,Llen),
  display_line2(Row),
  nl,
  N2 is N1+1,
  Counter1 is Counter +Llen,
  Lstart1 is Lstart +Llen,
  display_final_board(Board,N,N2,V,Total,Counter1,Lstart1,Llen).

  display_final_board(Board,N,N1,V,Total,Counter,Lstart,Llen):-
    nl.
  display_final_board([],_,_,[],_,_,_,_).



show_initial_board(Board,N,N1,V,H):-
  nl,
  nl,
  nl,
  write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n'),
  write('%%         Initial Board         %%\n'),
  write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n'),
  nl,
  write('    '),
  display_horizontal(H,N,N1),
  display_initial_board(Board,N,N1,V).

show_final_board(Board,V,H,N,N1,Lstart,Llen,Total,Counter):-
  nl,
  nl,
  nl,
  write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n'),
  write('%%          Final Board          %%\n'),
  write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n'),
  nl,
  write('    '),
  display_horizontal_final(H,N,N1),
  display_final_board(Board,N,N1,V,Total,Counter,Lstart,Llen).





translate(p1,'01').
translate(p2,'02').
translate(p3,'03').
translate(p4,'04').
translate(p5,'05').
translate(p6,'06').
translate(p7,'07').
translate(p8,'08').
translate(p9,'09').
translate(p10,'10').
translate(p11,'11').
translate(p12,'12').
translate(p13,'13').
translate(p14,'14').
translate(p15,'15').
translate(p16,'16').
translate(p17,'17').
translate(p18,'18').
translate(p19,'19').
translate(p20,'20').
translate(p21,'21').
translate(p22,'22').
translate(p23,'23').
translate(p24,'24').
translate(p25,'25').
translate(p26,'26').
translate(p27,'27').
translate(p28,'28').
translate(p29,'29').
translate(p30,'30').
translate(p31,'31').
translate(p32,'32').
translate(p33,'33').
translate(p34,'34').
translate(p35,'35').
translate(p36,'36').
translate(p37,'37').
translate(p38,'38').
translate(p39,'39').
translate(p40,'40').
translate(p41,'41').
translate(p42,'42').
