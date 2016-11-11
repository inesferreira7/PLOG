:- use_module(library(lists)).

board_1([
			[a,queen,queen,drone,vazio],
			[b,queen,drone,pawn,vazio],
			[c,drone,pawn,pawn,vazio],
			[d,vazio,vazio,pawn,vazio]
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

coordenates(Letter,Number,Piece):-
				line_board(Letter,X),
				Index is (Number-1),
				nth0(Index,X,Piece).

convert(Letter,Index):-
			(
			(
			Letter = a -> Index = 1;
			Letter = b -> Index = 2;
			Letter = c -> Index = 3;
			Letter = d -> Index = 4;
			Letter = e -> Index = 5;
			Letter = f -> Index = 6;
			Letter = g -> Index = 7;
			Letter = h -> Index = 8)
			; write('Letra invalida')
			).

convert_to_letter(Index,Letter):-
			(
			(
			Index = 1 -> Letter = a;
			Index = 2 -> Letter = b;
			Index = 3 -> Letter = c;
			Index = 4 -> Letter = d;
			Index = 5 -> Letter = e;
			Index = 6 -> Letter = f;
			Index = 7 -> Letter = g;
			Index = 8 -> Letter = h)
			; write('Numero invalido')
			).

inside_board(X,Index):-
				(
				(X<1 ; X>4; Index>8; Index<1)
				-> write('Coordenadas invalidas \n')
				; write('Coordenadas validas \n')
				).

check_drone_position(Xi,Yi,Xf,Yf):-
				convert(Yi,Indexi),
				convert(Yf,Indexf),
				inside_board(Xf,Indexf),
				(
				(Xf - Xi > 2 ; Indexf - Indexi > 2 ; Xi - Xf > 2 ; Indexi - Indexf > 2)
				-> (write('Jogada invalida para drone \n'),!)
				; write('Jogada valida para drone \n')


check_pawn_position(Xi,Yi,Xf,Yf):-
				convert(Yi,Indexi),
				convert(Yf,Indexf),
				inside_board(Xf,Indexf),
				Dx is (Xf - Xi),
				Dy is (Indexf - Indexi),
				(
				((Dx = -1 , Dy = -1) ; (Dx = -1 , Dy = 1) ; (Dx = 1 , Dy = 1) ; (Dx = 1 , Dy = -1))
				-> write('Jogada valida para peao\n')
				; write('Jogada invalida para peao\n')
				).

check_queen_position(Xi,Yi,Xf,Yf):-
				inside_board(Xf,Yf),
				convert(Yi,Indexi),
				convert(Yf,Indexf),
				Dx is abs(Xf - Xi),
				Dy is abs(Indexf - Indexi),
				(
				((Xi = Xf , Indexi \= Indexf) ; (Xi \= Xf , Indexi = Indexf) ; Dx = Dy)
				->write('Jogada valida para rainha')
				; write('Jogada invalida para rainha')
				).

check_owner(Letter,X):-
				convert(Letter,Y),
				(
				Y =< 4 -> X is 1;
				Y > 4 -> X is 2
				).




check_piece(Letter, Number,X):-
				coordenates(Letter,Number,Piece),
				(
				(Piece = vazio) -> X is 3
				; check_owner(Letter,X)
				).



move_pawn(Xi,Yi,Xf,Yf,Bo):-
				check_pawn_position(Xi,Yi,Xf,Yf),
				check_piece(Yf,Xf,X),
				(
				X = 1 -> write('Tried to move to a position where there is a player 1 piece. ');
				X = 2 -> write('Peça do player 2 ');
				X = 3 -> (board_1(Bi) , convert(Yi, Numi), Indexi is (Numi - 1), replace(Bi,Indexi,Xi,vazio,Bint), convert(Yf,Numf), Indexf is (Numf - 1), replace(Bint,Indexf,Xf,pawn,Bo))

				).

check_path_drone(Xi,Yi,Xf,Yf,Piece,Piece2):-
				convert(Yi,Indexi),
				convert(Yf,Indexf),
				Dx is abs(Xf-Xi),
				Dy is abs(Indexf-Indexi),
				(
				Dx = 1  -> (Piece2 is 3 ,  check_piece(Yf,Xf,Piece));
				Dy = 1 -> (Piece2 is 3 , check_piece(Yf,Xf,Piece));
				(Dx = 2, Xf > Xi)-> (check_piece(Yf,Xf,Piece), X1 is (Xf-1), check_piece(Yf,X1,Piece2));
				(Dx = 2, Xf < Xi)-> (check_piece(Yf,Xf,Piece), X1 is (Xf+1), check_piece(Yf,X1,Piece2));
				(Dy = 2, Indexf > Indexi)-> (check_piece(Yf,Xf,Piece), Y1 is (Indexf-1), convert_to_letter(Y1,L), check_piece(L,Xf,Piece2));
				(Dy = 2, Indexf < Indexi)-> (check_piece(Yf,Xf,Piece), Y1 is (Indexf+1), convert_to_letter(Y1,L), check_piece(L,Xf,Piece2))
				).

move_drone(Xi,Yi,Xf,Yf,Bo):-
				check_drone_position(Xi,Yi,Xf,Yf),
				check_path_drone(Xi,Yi,Xf,Yf,P1,P2),
				(
				(P1 = 1 ; P2 = 1) -> write('path with pieces (1)');
				(P1 = 2 ; P2 = 2) -> write('path with pieces (2)');
				(P1 = 3 , P2 = 3 )-> (write('empty path, move bitch'), board_1(Bi), convert(Yi, Numi), Indexi is (Numi - 1), replace(Bi,Indexi,Xi,vazio,Bint), convert(Yf, Numf), Indexf is (Numf - 1), replace(Bint,Indexf,Xf,drone,Bo))

				).

replace( L , X , Y , Z , R ) :-
				append(RowPfx,[Row|RowSfx],L),
				length(RowPfx,X) ,
				append(ColPfx,[_|ColSfx],Row) ,
				length(ColPfx,Y) ,
				append(ColPfx,[Z|ColSfx],RowNew) ,
				append(RowPfx,[RowNew|RowSfx],R)
				.

test(X,Y,Z,R):- board_1(B), replace(B,X,Y,Z,R).


play_game(X,Y,Z,S):- numbers(Z), board_1(X), board_2(Y), linha(S), display_board_numbers(Z), display_board_separa(S), display_board_1(X), display_board_separa(S), display_board_2(Y).
