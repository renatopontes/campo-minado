:- use_module(library(random)).
:- use_module(gera_jogo).

:- dynamic valor/3.
:- dynamic jogo_encerrado/0.
:- dynamic tem_flag/2.

inicio :- 	get_tamanho(TAM),
			random_between(1,TAM,I),
			random_between(1,TAM,J),
			posicao(I,J),
			escolhe_proxima.

heuristica :- 	get_tamanho(TAM),
				random_between(1,TAM,I),
				random_between(1,TAM,J),
				not(valor(I,J,_)), !,
				posicao(I,J),
				escolhe_proxima.
heuristica :-	heuristica.

escolhe_proxima :- 	ensure_loaded(['jogo']),
					valor(I, J, K),
					K > 0,
					n_flags(I, J, Nflags),
					n_fechadas(I, J, Nfechadas),
					descobrir_minas(I, J, K, Nflags, Nfechadas),
					descobrir_seguras(I, J, K, Nflags),
					fail.

escolhe_proxima :- heuristica.

%% escolhe_proxima :- total_fechadas(Tfechadas),
%% 				   total_flags(Tflags),
%% 				   Tfechadas = Tflags, !,
%% 				   writeln('Ganhou!'),
%% 				   halt.

%% escolhe_proxima :- true.


descobrir_minas(I, J, K, Nflags, Nfechadas) :- 	Nduvidas is Nfechadas - Nflags,
												Nfaltam is K - Nflags,
												Nduvidas = Nfaltam, !,
												marca_flags(I, J).
descobrir_minas(_, _, _, _, _).


descobrir_seguras(I, J, K, Nflags) :- 	Nflags = K, !,
										abre_vizinhos(I, J).
descobrir_seguras(_, _, _, _).


%% True se N é o número de casas com flag em torno da casa (I, J).
n_flags(I, J, N) :- n_flags(I, J, N, [-1, -1, -1, 0, 1, 1, 1, 0], [-1,  0,  1, 1, 1, 0, -1, -1]).
n_flags(_, _, 0, [], []) :- !.
n_flags(I, J, N, [DI|L1], [DJ|L2]) :- 	NI is I + DI, NJ is J + DJ,
										get_tamanho(TAM),
										NI > 0, NI =< TAM,
										NJ > 0, NJ =< TAM,
										tem_flag(NI, NJ), !,
										n_flags(I, J, W, L1, L2),
										N is W + 1.

n_flags(I, J, N, [_|L1], [_|L2]) :- n_flags(I, J, N, L1, L2).

%% True se N é o número de vizinhos fechados em torno da casa (I,J).
n_fechadas(I, J, N) :- n_fechadas(I, J, N, [-1, -1, -1, 0, 1, 1, 1, 0], [-1,  0,  1, 1, 1, 0, -1, -1]).
n_fechadas(_, _, 0, [], []) :- !.
n_fechadas(I, J, N, [DI|L1], [DJ|L2]) :- 	NI is I + DI, NJ is J + DJ,
											get_tamanho(TAM),
											NI > 0, NI =< TAM,
											NJ > 0, NJ =< TAM,
											not(valor(NI,NJ,_)), !,
											n_fechadas(I, J, W, L1, L2),
											N is W + 1.
n_fechadas(I, J, N, [_|L1], [_|L2]) :- n_fechadas(I, J, N, L1, L2).


%% True se T é o número total de casas com flag no tabuleiro.
total_flags(T) :- 	ensure_loaded(['jogo']),
					total_flags(1, 1, T).
total_flags(I, _, 0) :- get_tamanho(TAM), I > TAM, !.
total_flags(I, J, T) :- tem_flag(I, J), !,
						proxima_casa(I, J, NI, NJ),
						total_flags(NI, NJ, K),
						T is K + 1.
total_flags(I, J, T) :- proxima_casa(I, J, NI, NJ),
						total_flags(NI, NJ, T).

%% True se T é o número total de casas fechadas no tabuleiro.
total_fechadas(T) :- 	ensure_loaded(['jogo']),
						total_fechadas(1, 1, T).
total_fechadas(I, _, 0) :- 	get_tamanho(TAM), I > TAM, !.
total_fechadas(I, J, T) :- 	not(valor(I, J, _)), !,
							proxima_casa(I, J, NI, NJ),
							total_fechadas(NI, NJ, K),
							T is K + 1.
total_fechadas(I, J, T) :- 	proxima_casa(I, J, NI, NJ),
							total_fechadas(NI, NJ, T).


%% True se (NI, NJ) é a casa que segue (I, J).
proxima_casa(I, J, NI, 1) :- 	get_tamanho(TAM),
								J = TAM, !,
								NI is I + 1.

proxima_casa(I, J, I, NJ) :- 	NJ is J + 1.

marca_flags(I, J) :- marca_flags(I, J, [-1, -1, -1, 0, 1, 1, 1, 0], [-1,  0,  1, 1, 1, 0, -1, -1]).
marca_flags(_, _, [], []) :- !.
marca_flags(I, J, [DI|L1], [DJ|L2]) :- 	NI is I + DI, NJ is J + DJ,
										get_tamanho(TAM),
										NI > 0, NI =< TAM,
										NJ > 0, NJ =< TAM,
										not(valor(NI, NJ, _)),
										not(tem_flag(NI, NJ)), !,
										asserta(tem_flag(NI,NJ)),
										swritef(StrAux, '(%d,%d)', [NI, NJ]),
										write(StrAux),
										marca_flags(I, J, L1, L2).

marca_flags(I, J, [_|L1], [_|L2]) :- marca_flags(I, J, L1, L2).

abre_vizinhos(I, J) :- abre_vizinhos(I, J, [-1, -1, -1, 0, 1, 1, 1, 0], [-1,  0,  1, 1, 1, 0, -1, -1]).
abre_vizinhos(_, _, [], []) :- !.
abre_vizinhos(I, J, [DI|L1], [DJ|L2]) :- 	NI is I + DI, NJ is J + DJ,
											get_tamanho(TAM),
											NI > 0, NI =< TAM,
											NJ > 0, NJ =< TAM,
											not(valor(NI, NJ, _)),
											not(tem_flag(NI, NJ)), !,
											posicao(NI, NJ),
											ensure_loaded(['jogo']),
											abre_vizinhos(I, J, L1, L2).

abre_vizinhos(I, J, [_|L1], [_|L2]) :- abre_vizinhos(I, J, L1, L2).