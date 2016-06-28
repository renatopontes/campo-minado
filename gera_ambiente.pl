inicio :- 	open('ambiente.pl', write, Stream),
			['mina'],
			escreve_valores(Stream, 1, 1),
			close(Stream),
			halt.

%% Escreve os valores em 'ambiente.pl'
escreve_valores(_, I, _) :- tamanho(TAM), W is TAM + 1, I = W.
escreve_valores(Stream, I, J) :- 	not(mina(I,J)), !, 
									calcula_adj(I, J, N),
									swritef(StrAux, 'valor(%d,%d,%d).\n', [I, J, N]),
									write(Stream, StrAux),
									proxima_casa(I, J, NI, NJ),
									escreve_valores(Stream, NI, NJ).

escreve_valores(Stream, I, J) :- 	proxima_casa(I, J, NI, NJ),
									escreve_valores(Stream, NI, NJ).


%% True se (NI, NJ) é a casa que segue (I, J).
proxima_casa(I, J, NI, 1) :- 	tamanho(TAM),
								J = TAM, !,
								NI is I + 1.
								
proxima_casa(I, J, I, NJ) :- 	NJ is J + 1.

%% Retorna em N o número de minas adjacentes a casa (I,J).
%% False se tem mina em (I,J).
calcula_adj(I, J, N) :- calcula_adj(I, J, N, [-1, -1, -1, 0, 1, 1, 1, 0], [-1,  0,  1, 1, 1, 0, -1, -1]).
calcula_adj(_, _, 0, [], []).
calcula_adj(I, J, N, [DI|L1], [DJ|L2]) :- 	NI is I + DI, NJ is J + DJ,
											tamanho(TAM),
											NI > 0, NI =< TAM,
											NJ > 0, NJ =< TAM,
											mina(NI, NJ), !,
											calcula_adj(I, J, W, L1, L2),
											N is W + 1.

calcula_adj(I, J, N, [_|L1], [_|L2]) :- 	calcula_adj(I, J, N, L1, L2).



