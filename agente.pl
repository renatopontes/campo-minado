:- use_module(library(random)).
:- use_module(gera_jogo).

:- dynamic valor/3.
:- dynamic jogo_encerrado/0.
:- dynamic tem_flag/2.

inicio :- 	get_tamanho(TAM),
			random_between(1,TAM,I),
			random_between(1,TAM,J),
			posicao(I,J),
 			jogar.

%% heuristica([], []) :- !.
%% heuristica([(I, J, K)|L], LP) :-    vizinhos_com_flag(I, J, VizFlags),
%%                                     vizinhos_fechados(I, J, VizFec),
%%                                     VizSemFlag is VizFec - VizFlags,
%%                                     MinasDesc is K - VizFlags,
%%                                     Prob is MinasDesc/VizSemFlag,
%%                                     %% aplica_prob(Prob, I, J, LPV),
%%                                     heuristica(L, LPP),
%%                                     union(LPV, LPP, LP).

%% aplica_prob(Prob, I, J, LP)

%% inicio :-   writeln('HI'),
%% 			inicio,
%% 			writeln('HELLO').

%% heuristica :- 	get_tamanho(TAM),
%% 				random_between(1,TAM,I),
%% 				random_between(1,TAM,J),
%% 				not(valor(I,J,_)), !,
%% 				posicao(I,J),
%% 				escolhe_proxima.
%% heuristica :-	heuristica.

jogar :-	objetivo.
jogar :- 	['jogo'],
			lista_abertas(La),
			jogar_logica(La, NLa),
			['jogo'],
			jogar_heuris(NLa),
			jogar.

objetivo :-   	total_fechadas(Tfec),
            	total_flags(Tflag),
            	Tfec = Tflag, !,
            	writeln('Ganhou :D'), halt.

jogar_logica(L, NL) :- jogar_logica([], L, NL).
jogar_logica(Lista, Lista, Lista) :- !.
jogar_logica(_, Lista, NL) :-  	processa(Lista, NovaLista),
                        		jogar_logica(Lista, NovaLista, NL).

jogar_heuris(La) :- calcula_prob(La, PL),
					min_member((MinProb,_,_), PL),
					lista_minimos(PL, MinProb, MinPL),
					random_member((_, I, J), MinPL),
					posicao(I, J).

lista_minimos([], _, []) :- !.
lista_minimos([(Prob, I, J) | PLr], MinProb, MinPL) :-	Prob = MinProb, !,
														lista_minimos(PLr, MinProb, PLP),
														append([(Prob,I,J)], PLP, MinPL).
lista_minimos([_|PLr], MinProb, MinPL) :- lista_minimos(PLr, MinProb, MinPL).



calcula_prob([], []).
calcula_prob([(I, J, K) | La], PL) :- 	vizinhos_fechados(I, J, Nfech),
										vizinhos_com_flag(I, J, Nflag),
										Num is K - Nflag,
										Den is Nfech - Nflag,
										Prob is Num / Den,
										atribuir_prob(I, J, Prob, PLP),
										calcula_prob(La, PLR),
										union_prob(PLP, PLR, PL).

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

union_prob([], _, []) :- !.
union_prob([(Prob, I, J) | L1], L, L2) :- 	member((ProbAlt, I, J), L), !,
											max_member(ProbFinal, [Prob, ProbAlt]),
											union_prob(L1, L, L3),
											append([(ProbFinal, I, J)], L3, L2).
union_prob([Item | L1], L, L2) :- 	union_prob(L1, L, L3),
									append([Item], L3, L2).


lista_abertas(L) :- lista_abertas(1,1,L).
lista_abertas(I, _, []) :-  get_tamanho(TAM), I > TAM, !.
lista_abertas(I, J, L) :- 	valor(I, J, K),
                            K > 0, !,
							proxima_casa(I, J, NI, NJ),
							lista_abertas(NI, NJ, La),
							append([(I, J, K)], La, L).
lista_abertas(I, J, L) :- 	proxima_casa(I, J, NI, NJ),
							lista_abertas(NI, NJ, L).

processa([], []) :- !.
processa([(I,J,K)|L], NL) :-    vizinhos_com_flag(I, J, VizFlags),
                                VizFlags = K, !,
                                abre_vizinhos(I, J, LVizAbertos),
                                processa(L, NLP),
                                append(LVizAbertos, NLP, NL).

processa([(I,J,K)|L], NL) :-    vizinhos_fechados(I, J, VizFec),
                                VizFec = K, !,
                                flag_vizinhos(I, J),
                                processa(L, NL).

processa([Casa | L], NL) :-     processa(L, NLP),
                                append([Casa], NLP, NL).

% escolhe_proxima :- 	consult(['jogo']),
% 					valor(I, J, K),
% 					K > 0,
% 					n_flags(I, J, Nflags),
% 					n_fechadas(I, J, Nfechadas),
% 					swritef(StrAux, '(%d,%d)=(Nflags=%d,Nfechadas=%d)\n', [I, J, Nflags, Nfechadas]),
% 					write(StrAux),
% 					descobrir_minas(I, J, K, Nflags, Nfechadas),
% 					descobrir_seguras(I, J, K, Nflags),
% 					fail.

%% escolhe_proxima :- heuristica.

%% escolhe_proxima :- total_fechadas(Tfechadas),
%% 				   total_flags(Tflags),
%% 				   Tfechadas = Tflags, !,
%% 				   writeln('Ganhou!'),
%% 				   halt.

%% escolhe_proxima :- true.


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

flag_vizinhos(I, J) :- flag_vizinhos(I, J, [-1, -1, -1, 0, 1, 1, 1, 0], [-1,  0,  1, 1, 1, 0, -1, -1]).
flag_vizinhos(_, _, [], []) :- !.
flag_vizinhos(I, J, [DI|L1], [DJ|L2]) :- 	NI is I + DI, NJ is J + DJ,
										get_tamanho(TAM),
										NI > 0, NI =< TAM,
										NJ > 0, NJ =< TAM,
										not(valor(NI, NJ, _)),
										not(tem_flag(NI, NJ)), !,
										asserta(tem_flag(NI,NJ)),
										swritef(StrAux, 'A partir de (%d,%d) = mina em (%d,%d)\n', [I, J, NI, NJ]),
										write(StrAux),
										flag_vizinhos(I, J, L1, L2).

flag_vizinhos(I, J, [_|L1], [_|L2]) :- flag_vizinhos(I, J, L1, L2).

abre_vizinhos(I, J, L) :- abre_vizinhos(I, J, [-1, -1, -1, 0, 1, 1, 1, 0], [-1,  0,  1, 1, 1, 0, -1, -1], L).
abre_vizinhos(_, _, [], [], []) :- !.
abre_vizinhos(I, J, [DI|L1], [DJ|L2], L) :- 	NI is I + DI, NJ is J + DJ,
    											get_tamanho(TAM),
    											NI > 0, NI =< TAM,
    											NJ > 0, NJ =< TAM,
    											not(valor(NI, NJ, _)),
    											not(tem_flag(NI, NJ)), !,
    											swritef(StrAux, 'A partir de (%d,%d) = casa segura em (%d,%d)\n', [I, J, NI, NJ]),
    										    write(StrAux),
    											posicao(NI, NJ),
    											['jogo'],
    											valor(NI, NJ, K),
    											abre_vizinhos(I, J, L1, L2, LP),
    											append([(NI, NJ, K)], LP, L).

abre_vizinhos(I, J, [_|L1], [_|L2], L) :- abre_vizinhos(I, J, L1, L2, L).