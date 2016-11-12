:- use_module(library(lists)).

board([
			[a,queen,queen,drone,vazio],
			[b,queen,drone,pawn,vazio],
			[c,drone,pawn,pawn,vazio],
			[d,vazio,vazio,vazio,vazio],
      [e,vazio,vazio,vazio,vazio],
      [f,vazio,pawn,pawn,drone],
      [g,vazio,pawn,drone,queen],
      [h,vazio,drone,queen,queen]
			]).

board1vazio([
			[a,vazio,vazio,vazio,vazio],
			[b,vazio,vazio,vazio,vazio],
			[c,vazio,vazio,vazio,vazio],
			[d,vazio,vazio,vazio,vazio],
			[e,vazio,vazio,vazio,vazio],
			[f,vazio,pawn,pawn,drone],
			[g,vazio,pawn,drone,queen],
			[h,vazio,drone,queen,queen]
			]).

board2vazio([
			[a,queen,queen,drone,vazio],
			[b,queen,drone,pawn,vazio],
			[c,drone,pawn,pawn,vazio],
			[d,vazio,vazio,vazio,vazio],
			[e,vazio,vazio,vazio,vazio],
			[f,vazio,pawn,pawn,drone],
			[g,vazio,pawn,drone,queen],
			[h,vazio,drone,queen,queen]
			]).

numbers([
			[vazio,um,dois,tres,quatro]
			]).


display_board_numbers([B1|BS]):- nl, display_numbers(B1), display_board_numbers(BS).

display_board_numbers([]):-nl.

display_numbers([E1|ES]) :- translate(E1,V), write(V), write('   '), display_numbers(ES).

display_numbers([]).


display_board([L1|LS], N) :-
        nl,
        (
        N = 4 -> (write('  ----------------\n'),nl, display_line(L1));
        display_line(L1)),
        N1 is N+1,
        nl,
        display_board(LS, N1)
        .

display_board([],_).



display_all(Board):-numbers(N), display_board_numbers(N), display_board(Board,0).

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

line_board(Elem,X):-board(B), find_head(Elem,B,X).

removehead([_|Tail], Tail).


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
			Letter = h -> Index = 8);
      write('Letra invalida')
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


is_inside(Val, ValMin, ValMax):-
			Val >= ValMin,
			Val =< ValMax.


inside_board(Xi,Yi,Xf,Yf):-
			is_inside(Xi,1,4),
			is_inside(Xf,1,4),
			convert(Yi,Indexi),
			convert(Yf,Indexf),
			is_inside(Indexi,1,8),
			is_inside(Indexf,1,8).

check_drone_position(Xi,Yi,Xf,Yf,CanMove):-
				convert(Yi,Indexi),
				convert(Yf,Indexf),
				Dx is abs(Xf - Xi),
				Dy is abs(Indexf - Indexi),
				%inside_board(Xf,Indexf),
				(
				((Dx \= 0 , Dy \= 0) ; Dx > 2 ; Dy > 2)
				-> (CanMove is 1)
				; (CanMove is 0)
				).


check_pawn_position(Xi,Yi,Xf,Yf,CanMove):-
				convert(Yi,Indexi),
				convert(Yf,Indexf),
				%inside_board(Xf,Indexf),
				Dx is (Xf - Xi),
				Dy is (Indexf - Indexi),
				(
				((Dx = -1 , Dy = -1) ; (Dx = -1 , Dy = 1) ; (Dx = 1 , Dy = 1) ; (Dx = 1 , Dy = -1))
				-> (CanMove is 0)
				; (CanMove is 1)
				).

check_queen_position(Xi,Yi,Xf,Yf,CanMove):-
				convert(Yi,Indexi),
				convert(Yf,Indexf),
				%inside_board(Xf,Indexf),
				Dx is abs(Xf - Xi),
				Dy is abs(Indexf - Indexi),
				(
				((Xi = Xf , Indexi \= Indexf) ; (Xi \= Xf , Indexi = Indexf) ; Dx = Dy)
				->(CanMove is 0)
				; (CanMove is 1)
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

move_pawn(Xi,Yi,Xf,Yf):-
				coordenates(Yi,Xi,InitialPiece),
				(
				InitialPiece = pawn ->(check_pawn_position(Xi,Yi,Xf,Yf,CanMove),
				(
				CanMove = 0 -> (
				check_piece(Yf,Xf,X),
				(
				X = 1 -> write('path with pieces (1)');
				X = 2 -> write('path with pieces (2)');
				X = 3 -> (make_move(Xi,Yi,Xf,Yf,pawn,Bo), display_all(Bo))
				)
				);
				CanMove = 1 -> write('Impossible movement for the pawn, it will not move! \n')
				)
				);
				write('That piece you selected is not a pawn, you can not move it! ')
				).

move_drone(Xi,Yi,Xf,Yf):-
				coordenates(Yi,Xi,InitialPiece),
				(
				InitialPiece = drone ->(check_drone_position(Xi,Yi,Xf,Yf,CanMove),
				(
				CanMove = 0 -> (
				check_path_drone(Xi,Yi,Xf,Yf,P1,P2),
				(
				(P1 = 1 ; P2 = 1) -> write('path with pieces (1)');
				(P1 = 2 ; P2 = 2) -> write('path with pieces (2)');
				(P1 = 3 , P2 = 3 )-> make_move(Xi,Yi,Xf,Yf,drone,Bo), display_all(Bo)
				)
				);
				CanMove = 1 -> write('Impossible movement for the drone, it will not move! \n')
				)
				);
				write('That piece you selected is not a drone, you can not move it! ')
				)
				.

move_queen(Xi,Yi,Xf,Yf):-
				coordenates(Yi,Xi,InitialPiece),
				(
				InitialPiece = queen ->(check_queen_position(Xi,Yi,Xf,Yf),
				(
				CanMove = 0 ->(
				check_path_queen(Xi,Yi,Xf,Yf,Move),
				(
				Move = 1 -> write('path with pieces (1)');
				Move = 2 -> write('path with pieces (2)');
				Move = 3 -> make_move(Xi,Yi,Xf,Yf,queen,Bo), display_all(Bo)
				)
				);
				CanMove = 1 -> write('Impossible movement for the queen, it will not move! \n')
				)
				);
				write('That piece you selected is not a queen, you can not move it! ')
				)
				.

check_path_queen(Xi,Yi,Xf,Yf,Move):-
				convert(Yi,Indexi),
				convert(Yf,Indexf),
				Dx is abs(Xf - Xi),
				Dy is abs(Indexf - Indexi),
				NewDy is Indexf - Indexi,
				NewDx is Xf - Xi,
				/**Verificar o movimento da rainha no eixo do x **/
				(
				((Dx \= 0 , Dy = 0 )-> ((
										(Xf > Xi) -> (X is Xi+1);
										(Xf < Xi) -> (X is Xi-1)
										),
										check_piece(Yi,X,Piece),
										(
										(Piece = 1 )-> Move is 1;
										(Piece = 2) -> Move is 2;
										(Piece = 3 )-> check_path_queen(X,Yi,Xf,Yf,Move)
										)
										)
				);

				((Dy \= 0 , Dx = 0 )-> (
										(
										(Indexf > Indexi) -> (Y is Indexi+1);
										(Indexf < Indexi) -> (Y is Indexi-1)
										),
										convert_to_letter(Y,NewY),
										check_piece(NewY,Xi,Piece),
										(
										(Piece = 1 )-> Move is 1;
										(Piece = 2 ) -> Move is 2;
										(Piece = 3 )-> check_path_queen(Xi,NewY,Xf,Yf,Move)
										)
										)
				);
				%diagonais
				%direita
				((NewDx > 0) -> (
												(
												((NewDy > 0) -> ( (X is Xi+1) ,(Y is Indexi + 1)));  %baixo
												((NewDy < 0) -> ( (X is Xi+1) ,(Y is Indexi - 1)))	 %cima
												),
												convert_to_letter(Y,NewY),
												check_piece(NewY,X,Piece),
												(
												(Piece = 1 )-> Move is 1;
												(Piece = 2 ) -> Move is 2;
												(Piece = 3 )-> check_path_queen(X,NewY,Xf,Yf,Move)
												)
												)

				);
				%esquerda
				((NewDx < 0) -> (
												(
												((NewDy > 0) -> ( (X is Xi-1) ,(Y is Indexi + 1)));  %baixo
												((NewDy < 0) -> ( (X is Xi-1) ,(Y is Indexi - 1)))	 %cima
												),
												convert_to_letter(Y,NewY),
												check_piece(NewY,X,Piece),
												(
												(Piece = 1 )-> Move is 1;
												(Piece = 2 ) -> Move is 2;
												(Piece = 3 )-> check_path_queen(X,NewY,Xf,Yf,Move)
												)
												)

				);


				((NewDx = 0, NewDy = 0) -> Move is 3);
				((Dx = 0 , Dy = 0) -> Move is 3
				)
				)

				.



make_move(Xi,Yi,Xf,Yf,Piece,Bo):-
				board(Bi) ,
				convert(Yi, Numi),
				convert(Yf,Numf),
				Indexi is (Numi - 1),
				Indexf is (Numf - 1),
				replace(Bi,Indexi,Xi,vazio,Bint),
				replace(Bint,Indexf,Xf,Piece,Bo).

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


play_1(Xi,Yi,Xf,Yf):-
				coordenates(Yi,Xi,Piece),
				convert(Yi,Num),
				(
				Num > 4 -> write('Not your piece');
				(
				Piece = pawn -> move_pawn(Xi,Yi,Xf,Yf);
				Piece = drone -> move_drone(Xi,Yi,Xf,Yf);
				Piece = queen -> move_drone(Xi,Yi,Xf,Yf);
				Piece = vazio -> write('Nothing to move on those coordenates')
				)).

play_2(Xi,Yi,Xf,Yf):-
				coordenates(Yi,Xi,Piece),
				convert(Yi,Num),
				(
				Num =< 4 -> write('Not your piece');
				(
				Piece = pawn -> move_pawn(Xi,Yi,Xf,Yf);
				Piece = drone -> move_drone(Xi,Yi,Xf,Yf);
				Piece = queen -> move_drone(Xi,Yi,Xf,Yf);
				Piece = vazio -> write('Nothing to move on those coordenates')
				)).

verify_board_1(Board,Line,X) :-
				(
				Line = 5 -> X=1 %se x=1 quer dizer que está vazio
				);
				nth1(Line,Board,Elem),
				removehead(Elem,Elem1),
				(
				Elem1 \= [vazio,vazio,vazio,vazio] -> X = 0
				;
				Line1 is (Line + 1),
				verify_board_1(Board,Line1,X)
				).

verify_board_2(Board,Line,X) :-
				(
				Line = 9 -> X=1 %se x=1 quer dizer que está vazio
				);
				nth1(Line,Board,Elem),
				removehead(Elem,Elem1),
				(
				Elem1 \= [vazio,vazio,vazio,vazio] -> X = 0
				;
				Line1 is (Line + 1),
				verify_board_1(Board,Line1,X)
				).


ask_coordenates_1:-
				write('Initial x:'), nl,
				read(Xi),
				write('Initial y:'), nl,
				read(Yi),
				write('Final x:'), nl,
				read(Xf),
				write('Final y:'), nl,
				read(Yf),
				inside_board(Xi,Yi,Xf,Yf),
				play_1(Xi,Yi,Xf,Yf).

ask_coordenates_1:-
	write('Invalid coordenates, please insert new ones'), nl,
	ask_coordenates_1.

ask_coordenates_2:-
				write('Initial x:'), nl,
				read(Xi),
				write('Initial y:'), nl,
				read(Yi),
				write('Final x:'), nl,
				read(Xf),
				write('Final y:'), nl,
				read(Yf),
				inside_board(Xi,Yi,Xf,Yf),
				play_2(Xi,Yi,Xf,Yf).

	ask_coordenates_2:-
		write('Invalid coordenates, please insert new ones'), nl,
		ask_coordenates_2.

endGame(Board,X):-
			verify_board_1(Board,1,X),
			(X = 1 -> write('Player 2 win!!')),
			verify_board_2(Board,5,Y),
			(Y = 1 -> write('Player 1 win!!')).

isPar(N):- N mod 2 =:= 0.

play_game(N):-
		N1 is N+1,
		(
		isPar(N1) -> ask_coordenates_1;
		ask_coordenates_2
		),
		play_game(N1).

replace( L , X , Y , Z , R ) :-
				append(RowPfx,[Row|RowSfx],L),
				length(RowPfx,X) ,
				append(ColPfx,[_|ColSfx],Row) ,
				length(ColPfx,Y) ,
				append(ColPfx,[Z|ColSfx],RowNew) ,
				append(RowPfx,[RowNew|RowSfx],R)
				.
