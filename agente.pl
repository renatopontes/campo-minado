:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(gera_jogo).

:- dynamic valor/3.
:- dynamic jogo_encerrado/0.
:- dynamic tem_flag/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
inicio :- 	get_tamanho(TAM),
			random_between(1,TAM,I),
			random_between(1,TAM,J),
			posicao(I,J),
 			jogar.

jogar :-	objetivo.
jogar :- 	['jogo'],
			fronteira(La),
			jogar_logica(La, NLa),
			['jogo'],
			jogar_heuris(NLa),
			jogar.

objetivo :-   	total_fechadas(Tfec),
            	total_flags(Tflag),
            	Tfec = Tflag, !,
            	writeln('Ganhou :D'), halt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
jogar_logica(L, NL) :- jogar_logica([], L, NL).
jogar_logica(Lista, Lista, Lista) :- !.
jogar_logica(_, Lista, NL) :-  	processa(Lista),
								fronteira(NovaLista),
                        		jogar_logica(Lista, NovaLista, NL).

processa([]) :- !.
processa([(I,J,K)|L]) :-    vizinhos_com_flag(I, J, VizFlags),
                            VizFlags = K, !,
                            abre_vizinhos(I, J),
                            processa(L).

processa([(I,J,K)|L]) :-    vizinhos_fechados(I, J, VizFec),
                            VizFec = K, !,
                            flag_vizinhos(I, J),
                            processa(L).

processa([_ | L]) :-     processa(L).

flag_vizinhos(I, J) :- flag_vizinhos(I, J, [-1, -1, -1, 0, 1, 1, 1, 0], [-1,  0,  1, 1, 1, 0, -1, -1]).
flag_vizinhos(_, _, [], []) :- !.
flag_vizinhos(I, J, [DI|L1], [DJ|L2]) :- 	NI is I + DI, NJ is J + DJ,
										get_tamanho(TAM),
										NI > 0, NI =< TAM,
										NJ > 0, NJ =< TAM,
										not(valor(NI, NJ, _)),
										not(tem_flag(NI, NJ)), !,
										asserta(tem_flag(NI,NJ)),
										flag_vizinhos(I, J, L1, L2).

flag_vizinhos(I, J, [_|L1], [_|L2]) :- flag_vizinhos(I, J, L1, L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
jogar_heuris(La) :- calcula_prob(La, PL),
					PL = [_ | _],
					min_member((MinProb,_,_), PL),
					lista_minimos(PL, MinProb, MinPL),
					random_member((_, I, J), MinPL),
					posicao(I, J).
jogar_heuris(_).

calcula_prob([], []).
calcula_prob([(I, J, K) | La], PL) :- 	vizinhos_fechados(I, J, Nfech),
										vizinhos_com_flag(I, J, Nflag),
										Num is K - Nflag,
										Den is Nfech - Nflag,
										Prob is Num / Den,
										atribuir_prob(I, J, Prob, PLP),
										calcula_prob(La, PLR),
										union_prob(PLP, PLR, PL).

union_prob([], L2, L2) :- !.
union_prob([(Prob, I, J) | Lr], L2, L3) :- 	member((ProbAlt, I, J), L2), !,
											max_member(ProbFinal, [Prob, ProbAlt]),
											delete(L2, (_, I, J), L2mod),
											union_prob(Lr, L2mod, L3p),
											append([(ProbFinal, I, J)], L3p, L3).
union_prob([Item | Lr], L2, L3) :- 	union_prob(Lr, L2, L3p),
									append([Item], L3p, L3).


lista_minimos([], _, []) :- !.
lista_minimos([(Prob, I, J) | PLr], MinProb, MinPL) :-	Prob = MinProb, !,
														lista_minimos(PLr, MinProb, PLP),
														append([(Prob,I,J)], PLP, MinPL).
lista_minimos([_|PLr], MinProb, MinPL) :- lista_minimos(PLr, MinProb, MinPL).


atribuir_prob(I, J, Prob, PLP) :- atribuir_prob(I, J, Prob, PLP, [-1, -1, -1, 0, 1, 1, 1, 0], [-1,  0,  1, 1, 1, 0, -1, -1]).
atribuir_prob(_, _, _, [], [], []) :- !.
atribuir_prob(I, J, Prob, PLP, [DI|L1], [DJ|L2]) :-	NI is I + DI, NJ is J + DJ,
													get_tamanho(TAM),
													NI > 0, NI =< TAM,
													NJ > 0, NJ =< TAM,
													not(valor(NI,NJ,_)),
													not(tem_flag(NI,NJ)), !,
													atribuir_prob(I, J, Prob, PLPr, L1, L2),
													append([(Prob, NI, NJ)], PLPr, PLP).
atribuir_prob(I, J, Prob, PLP, [_|L1], [_|L2]) :- atribuir_prob(I, J, Prob, PLP, L1, L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fronteira(L) :- fronteira(1,1,L).
fronteira(I, _, []) :-  get_tamanho(TAM), I > TAM, !.
fronteira(I, J, L) :- 	valor(I, J, K),
						vizinhos_fechados(I, J, Nfech),
						vizinhos_com_flag(I, J, Nflag),
						K > 0, Nfech > 0, Nfech > Nflag, !,
						proxima_casa(I, J, NI, NJ),
						fronteira(NI, NJ, La),
						append([(I, J, K)], La, L).
fronteira(I, J, L) :- 	proxima_casa(I, J, NI, NJ),
							fronteira(NI, NJ, L).

%% True se N é o número de casas com flag em torno da casa (I, J).
vizinhos_com_flag(I, J, N) :- vizinhos_com_flag(I, J, N, [-1, -1, -1, 0, 1, 1, 1, 0], [-1,  0,  1, 1, 1, 0, -1, -1]).
vizinhos_com_flag(_, _, 0, [], []) :- !.
vizinhos_com_flag(I, J, N, [DI|L1], [DJ|L2]) :- 	NI is I + DI, NJ is J + DJ,
										get_tamanho(TAM),
										NI > 0, NI =< TAM,
										NJ > 0, NJ =< TAM,
										tem_flag(NI, NJ), !,
										vizinhos_com_flag(I, J, W, L1, L2),
										N is W + 1.

vizinhos_com_flag(I, J, N, [_|L1], [_|L2]) :- vizinhos_com_flag(I, J, N, L1, L2).

%% True se N é o número de vizinhos fechados em torno da casa (I,J).
vizinhos_fechados(I, J, N) :- vizinhos_fechados(I, J, N, [-1, -1, -1, 0, 1, 1, 1, 0], [-1,  0,  1, 1, 1, 0, -1, -1]).
vizinhos_fechados(_, _, 0, [], []) :- !.
vizinhos_fechados(I, J, N, [DI|L1], [DJ|L2]) :- 	NI is I + DI, NJ is J + DJ,
											get_tamanho(TAM),
											NI > 0, NI =< TAM,
											NJ > 0, NJ =< TAM,
											not(valor(NI,NJ,_)), !,
											vizinhos_fechados(I, J, W, L1, L2),
											N is W + 1.
vizinhos_fechados(I, J, N, [_|L1], [_|L2]) :- vizinhos_fechados(I, J, N, L1, L2).


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

abre_vizinhos(I, J) :- abre_vizinhos(I, J, [-1, -1, -1, 0, 1, 1, 1, 0], [-1,  0,  1, 1, 1, 0, -1, -1]).
abre_vizinhos(_, _, [], []) :- !.
abre_vizinhos(I, J, [DI|L1], [DJ|L2]) :- 	NI is I + DI, NJ is J + DJ,
											get_tamanho(TAM),
											NI > 0, NI =< TAM,
											NJ > 0, NJ =< TAM,
											not(valor(NI, NJ, _)),
											not(tem_flag(NI, NJ)), !,
											posicao(NI, NJ),
											['jogo'],
											abre_vizinhos(I, J, L1, L2).

abre_vizinhos(I, J, [_|L1], [_|L2]) :- abre_vizinhos(I, J, L1, L2).