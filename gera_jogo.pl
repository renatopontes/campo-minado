%% TODO: Usar reconsult('jogo.pl')
:- dynamic aberto/2.
:- dynamic mina/2.
:- dynamic valor/3.
:- dynamic tamanho/1.
:- dynamic arquivo_aberto/0.

posicao(I, J) :- 	not(arquivo_aberto), !,
					open('jogo.pl', write, Stream),
					assert(arquivo_aberto),
					posicao(I, J, Stream).

posicao(I, J) :- 	open('jogo.pl', append, Stream),
					posicao(I, J, Stream).
					

posicao(I, J, Stream) :-	['jogo'],
							['mina'],
							['ambiente'],
							abrir_casa(I,J,Stream),
							close(Stream).

abrir_casa(I, J, _) :- 		aberto(I, J), !.

abrir_casa(I, J, Stream) :- mina(I,J), !,
							assert(aberto(I,J)),
							escreve_resposta(Stream, 'jogo_encerrado').

abrir_casa(I, J, Stream) :- valor(I, J, K), K > 0, !,
							assert(aberto(I,J)),
							swritef(StrAux, 'valor(%d,%d,%d)', [I, J, K]),
							escreve_resposta(Stream, StrAux).

abrir_casa(I, J, Stream) :- swritef(StrAux, 'valor(%d,%d,0)', [I, J]),
							escreve_resposta(Stream, StrAux),
							assert(aberto(I,J)),
							expandir_casa(I, J, Stream).

expandir_casa(I, J, Stream) :- expandir_casa(I, J, [-1, -1, -1, 0, 1, 1, 1, 0], [-1,  0,  1, 1, 1, 0, -1, -1], Stream).
expandir_casa(_, _, [], [], _) :- !.
expandir_casa(I, J, [DI|L1], [DJ|L2], Stream) :- 	NI is I + DI, NJ is J + DJ,
											tamanho(TAM),
											NI > 0, NI =< TAM,
											NJ > 0, NJ =< TAM,
											not(aberto(NI, NJ)), !,
											abrir_casa(NI, NJ, Stream),
											expandir_casa(I, J, L1, L2, Stream).

expandir_casa(I, J, [_|L1], [_|L2], Stream) :- 	expandir_casa(I, J, L1, L2, Stream).

escreve_resposta(Stream, String) :- atom_concat(String, '.\n', StringFile),
									write(Stream, StringFile),
									writeln(String).
