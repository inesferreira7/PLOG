board_1([
			[a,queen,queen,drone,vazio],
			[b,queen,drone,pawn,vazio],
			[c,drone,pawn,pawn,vazio],
			[d,vazio,vazio,vazio,vazio]
			]).

board_2([	[e,vazio,vazio,vazio,vazio],
			[f,vazio,pawn,pawn,drone],
			[g,vazio,pawn,drone,queen],
			[h,vazio,drone,queen,queen]
			]).

numbers([
			[vazio,um,dois,tres,quatro]
			]).

linha([		[vazio,linha,linha,linha,linha]
			]).


display_board_numbers([B1|BS]):- nl, display_numbers(B1), display_board_numbers(BS).

display_board_numbers([]):-nl.

display_numbers([E1|ES]) :- translate(E1,V), write(V), write('   '), display_numbers(ES).

display_numbers([]).



display_board_separa([B1|BS]):- nl, display_separa(B1), display_board_separa(BS).

display_board_separa([]):-nl.

display_separa([E1|ES]) :- translate(E1,V), write(V), write('   '), display_separa(ES).

display_separa([]).




display_board_1([L1|LS]) :- nl, display_line(L1), nl, display_board_1(LS).

display_board_1([]).

display_board_2([L1|LS]) :- nl, display_line(L1), nl, display_board_2(LS).

display_board_2([]):-nl.

display_line([E1|ES]) :- translate(E1,V), write(V), write(' | '), display_line(ES).

display_line([]).



translate(vazio,' ').
translate(linha,'-').
translate(queen,'0').
translate(drone,'*').
translate(pawn,'.').
translate(a,'A').
translate(b,'B').
translate(c,'C').
translate(d,'D').
translate(e,'E').
translate(f,'F').
translate(g,'G').
translate(h,'H').
translate(um,'1').
translate(dois,'2').
translate(tres,'3').
translate(quatro,'4').

%%%%%% Find line with y=Letter %%%%%%

head(Elem, [Head|Rs],Rs) :- Elem = Head.
find_head(Elem, [Line|Rest],X):- head(Elem,Line,X); find_head(Elem,Rest,X).

head_board_1(Elem,X):-board_1(B), find_head(Elem,B,X).
head_board_2(Elem,X):-board_2(B), find_head(Elem,B,X).

line_board(Elem,X):-
					(
					Elem = a -> head_board_1(Elem,X);
					Elem = b -> head_board_1(Elem,X);
					Elem = c -> head_board_1(Elem,X);
					Elem = d -> head_board_1(Elem,X);
					Elem = e -> head_board_2(Elem,X);
					Elem = f -> head_board_2(Elem,X);
					Elem = g -> head_board_2(Elem,X);
					Elem = h -> head_board_2(Elem,X)
					).



play_game(X,Y,Z,S):- numbers(Z), board_1(X), board_2(Y), linha(S), display_board_numbers(Z), display_board_separa(S), display_board_1(X), display_board_separa(S), display_board_2(Y).
