:- use_module(library(random)). % Para geração de números aleatórios
:- use_module(library(lists)). 	% Para operações com lista
:- use_module(gera_jogo). 		% posicao/2 e get_tamanho/1

% Predicados que aparecem em tempo de execução.
:- dynamic valor/3.
:- dynamic jogo_encerrado/0.
:- dynamic tem_flag/2. % True se (I,J) foi marcada com flag

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Jogada inicial aleatória
inicio :- 	get_tamanho(TAM),
			random_between(1,TAM,I), % de random.
			random_between(1,TAM,J), % de random.
			posicao(I,J),
 			jogar.

% Loop principal
% Verifica se ganhou o jogo. Se não, joga logicamente e por heurisitica.
jogar :-	objetivo.
jogar :- 	['jogo'],
			fronteira(La), % Verificar seção 5 do README.md
			jogar_logica(La, NLa),
			['jogo'],
			jogar_heuris(NLa),
			jogar.

% O agente ganha se todas as casas fechadas são minas.
objetivo :-   	total_fechadas(Tfec),
            	total_flags(Tflag),
            	Tfec = Tflag, !,
            	writeln('Ganhou :D'), halt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Loop para jogar logicamente.
% Processa a fronteira e a reconstrói a cada iteração.
% Recebe a fronteira L e gera uma nova fronteira NL, resultado
% do processamento de L.
jogar_logica(L, NL) :- jogar_logica([], L, NL).
jogar_logica(Lista, Lista, Lista) :- !.
jogar_logica(_, Lista, NL) :-  	processa(Lista),
								fronteira(NovaLista),
                        		jogar_logica(Lista, NovaLista, NL).

% Verifica, para cada elemento da fronteira, se é possível afirmar com certeza
% que casas tem mina e que casas são seguras.
processa([]) :- !.
% Caso 1: K = VizFlags => Todas as casas adj. sem flag são seguras.
processa([(I,J,K)|L]) :-    vizinhos_com_flag(I, J, VizFlags),
                            VizFlags = K, !,
                            abre_vizinhos(I, J),
                            processa(L).
% Caso 2: K = VizFec => Todas as casas adj. podem ser marcadas com flag.
processa([(I,J,K)|L]) :-    vizinhos_fechados(I, J, VizFec),
                            VizFec = K, !,
                            flag_vizinhos(I, J),
                            processa(L).
% Caso 3: Não é possível fazer nada, tente o próximo elemento.
processa([_ | L]) :-     processa(L).

% Para cada vizinho fechado de (I,J) que não tem flag,  marque-o com flag.
flag_vizinhos(I, J) :- flag_vizinhos(I, J, 	[-1, -1, -1, 0, 1, 1, 1, 0],
											[-1,  0,  1, 1, 1, 0, -1, -1]).
% Clausula base: todos os vizinhos foram visitados.
flag_vizinhos(_, _, [], []) :- !.
% Se (I,J) deve ser marcado com flag...
flag_vizinhos(I, J, [DI|L1], [DJ|L2]) :- 	NI is I + DI, NJ is J + DJ,
										get_tamanho(TAM),
										NI > 0, NI =< TAM,
										NJ > 0, NJ =< TAM,
										not(valor(NI, NJ, _)),
										not(tem_flag(NI, NJ)), !,
										asserta(tem_flag(NI,NJ)),
										flag_vizinhos(I, J, L1, L2).
% ... Se não, tente o próximo vizinho de (I,J).
flag_vizinhos(I, J, [_|L1], [_|L2]) :- flag_vizinhos(I, J, L1, L2).

% Para cada vizinho fechado de (I,J), abra-o se não estiver com flag.
abre_vizinhos(I, J) :- abre_vizinhos(I, J, 	[-1, -1, -1, 0, 1, 1, 1, 0],
											[-1,  0,  1, 1, 1, 0, -1, -1]).
% Caso base.
abre_vizinhos(_, _, [], []) :- !.
% Se (I,J) deve ser aberto...
abre_vizinhos(I, J, [DI|L1], [DJ|L2]) :- 	NI is I + DI, NJ is J + DJ,
											get_tamanho(TAM),
											NI > 0, NI =< TAM,
											NJ > 0, NJ =< TAM,
											not(valor(NI, NJ, _)),
											not(tem_flag(NI, NJ)), !,
											posicao(NI, NJ),
											['jogo'],
											abre_vizinhos(I, J, L1, L2).
% ... Do contrário, tente o próximo vizinho.
abre_vizinhos(I, J, [_|L1], [_|L2]) :- abre_vizinhos(I, J, L1, L2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A partir da fronteira La, faz uma jogada baseada nas probabilidades locais
% de cada casa ter mina.
jogar_heuris(La) :- calcula_prob(La, PL),
					PL = [_ | _], % Certifica que PL não é vazia.
					min_member((MinProb,_,_), PL), % de lists.
					lista_minimos(PL, MinProb, MinPL),
					random_member((_, I, J), MinPL), % de random.
					posicao(I, J).
jogar_heuris(_).

% Para cada casa da fronteira, atribui a seus vizinhos fechados sem flag uma
% probabilidade local de ter mina, gerando uma lista que associa cada candidato
% a ser aberto com uma probabilidade.
calcula_prob([], []).
calcula_prob([(I, J, K) | La], PL) :- 	vizinhos_fechados(I, J, Nfech),
										vizinhos_com_flag(I, J, Nflag),
										Num is K - Nflag,
										Den is Nfech - Nflag,
										Prob is Num / Den,
										atribuir_prob(I, J, Prob, PLP),
										calcula_prob(La, PLR),
										union_prob(PLP, PLR, PL).

% Junta as probabilidades calculadas para uma casa com a lista global,
% resolvendo eventuais conflitos.
% A estimativa mais pessimista é sempre colocada na lista final, L3.
union_prob([], L2, L2) :- !.
union_prob([(Prob, I, J) | Lr], L2, L3) :- 	member((ProbAlt, I, J), L2), !,
											max_member(ProbFinal, [Prob, ProbAlt]),
											delete(L2, (_, I, J), L2mod),
											union_prob(Lr, L2mod, L3p),
											append([(ProbFinal, I, J)], L3p, L3).
union_prob([Item | Lr], L2, L3) :- 	union_prob(Lr, L2, L3p),
									append([Item], L3p, L3).

% Recebe uma lista casas associadas a probabilidades e a menor probabilidade
% MinProb e mantém na lista final MinPL apenas os elementos que tem
% probabilidade mínima.
lista_minimos([], _, []) :- !.
lista_minimos([(Prob, I, J) | PLr], MinProb, MinPL) :-	Prob = MinProb, !,
														lista_minimos(PLr, MinProb, PLP),
														append([(Prob,I,J)], PLP, MinPL).
lista_minimos([_|PLr], MinProb, MinPL) :- lista_minimos(PLr, MinProb, MinPL).


% Atribui aos vizinhos de (I,J) a probabilidade Prob, e retorna uma lista PLP
% de vizinhos associados a Prob, na forma [(Prob, Vi, Vj), ...].
atribuir_prob(I, J, Prob, PLP) :- atribuir_prob(I, J, Prob, PLP,
												[-1, -1, -1, 0, 1, 1, 1, 0],
												[-1,  0,  1, 1, 1, 0, -1, -1]).
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
% Para cada casa aberta do tabuleiro, coloque-a na lista de saída L se a casa é
% capaz de dar informações sobre que casa deve ser aberta em jogadas futuras.
% (Ver seção 5 do README.md)
fronteira(L) :- fronteira(1,1,L).
fronteira(I, _, []) :-  get_tamanho(TAM), I > TAM, !.
fronteira(I, J, L) :- 	valor(I, J, K),
						vizinhos_fechados(I, J, Nfech),
						vizinhos_com_flag(I, J, Nflag),
						K > 0, Nfech > 0, Nfech > Nflag, !,
						proxima_casa(I, J, NI, NJ),
						fronteira(NI, NJ, La),
						append([(I, J, K)], La, L). % de lists.
fronteira(I, J, L) :- 	proxima_casa(I, J, NI, NJ),
						fronteira(NI, NJ, L).

%% True se N é o número de casas com flag em torno da casa (I, J).
vizinhos_com_flag(I, J, N) :- vizinhos_com_flag(I, J, N,
												[-1, -1, -1, 0, 1, 1, 1, 0],
												[-1,  0,  1, 1, 1, 0, -1, -1]).
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
vizinhos_fechados(I, J, N) :- vizinhos_fechados(I, J, N,
												[-1, -1, -1, 0, 1, 1, 1, 0],
												[-1,  0,  1, 1, 1, 0, -1, -1]).
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
% Caso 1: Ultima coluna da linha => vai para a primeira coluna da linha
% seguinte. (NI = I + 1 e NJ = 1)
proxima_casa(I, J, NI, 1) :- 	get_tamanho(TAM),
								J = TAM, !,
								NI is I + 1.
% Caso 2: Continua na mesma linha, só incrementa J.
proxima_casa(I, J, I, NJ) :- 	NJ is J + 1.