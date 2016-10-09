board([		[queen,queen,drone,vazio],
			[queen,drone,pawn,vazio],
			[drone,pawn,pawn,vazio],
			[vazio,vazio,vazio,vazio],
			[vazio,vazio,vazio,vazio],
			[vazio,pawn,pawn,drone],
			[vazio,pawn,drone,queen],
			[vazio,drone,queen,queen]
			]).
	 
display_board([L1|LS]) :- display_line(L1), nl, display_board(LS).

display_board([]):-nl.

display_line([E1|ES]) :- traduz(E1,V), write(V), write(' | '), display_line(ES).

display_line([]).

traduz(vazio,' ').
traduz(queen,'Q').
traduz(drone,'D').
traduz(pawn,'P').

play_game(X):-board(X), display_board(X).