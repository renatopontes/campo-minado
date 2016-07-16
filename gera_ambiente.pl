:- dynamic mina/2.

%% A partir do mina.pl descreve o ambiente no arquivo ambiente.pl no formato valor(I,J,K),
%% onde (I,J) são as coordenadas de uma casa no tabuleiro e K o número de minas existentes
%% nas adjacências dessa casa.
inicio :- 	open('ambiente.pl', write, Stream),
			['mina'],
			escreve_valores(Stream, 1, 1),
			close(Stream),
			halt.

%% Escreve os valores em 'ambiente.pl' percorrendo os elementos adjacentes caso a casa
%% atual não tenha mina.
%% Quando a linha (I) for igual ao tamanho de uma das dimensões do tabuleiro mais uma
%% unidade, isto significa que já percorremos todo o tabuleiro.
escreve_valores(_, I, _) :- tamanho(TAM), W is TAM + 1, I = W.
escreve_valores(Stream, I, J) :- 	not(mina(I,J)), !,
									calcula_adj(I, J, N),
									swritef(StrAux, 'valor(%d,%d,%d).\n', [I, J, N]),
									write(Stream, StrAux),
									proxima_casa(I, J, NI, NJ),
									escreve_valores(Stream, NI, NJ).
%% Caso a casa tenha mina, simplesmente vá para a próxima casa
escreve_valores(Stream, I, J) :- 	proxima_casa(I, J, NI, NJ),
									escreve_valores(Stream, NI, NJ).


%% True se (NI, NJ) é a casa que segue (I, J).
%% Caso em que estamos em uma casa no fim de uma linha e queremos ir para a primeira casa
%% da próxima linha.
proxima_casa(I, J, NI, 1) :- 	tamanho(TAM),
								J = TAM, !,
								NI is I + 1.
%% Caso contrário, em que estamos em alguma casa que não seja a última de uma linha e só
%% gostaríamos de andar uma unidade a frente na coluna.
proxima_casa(I, J, I, NJ) :- 	NJ is J + 1.

%% Retorna em N o número de minas adjacentes a casa (I,J).
calcula_adj(I, J, N) :- calcula_adj(I, J, N, [-1, -1, -1, 0, 1, 1, 1, 0], [-1,  0,  1, 1, 1, 0, -1, -1]).
%% Caso base onde já percorremos todos os vizinhos, ou seja, o vetor de deslocamento ja foi totalmente
%% percorrido, deixando sua lista vazia (... [],[]). Recursivamente, enquanto estivermos voltando aos vizinhos,
%% vamos calculando em N a soma acumulada de minas em volta. Verificamos também se as coordenadas (NI,NJ)
%% estão dentro limites do tabuleiro.
calcula_adj(_, _, 0, [], []) :- !.
calcula_adj(I, J, N, [DI|L1], [DJ|L2]) :- 	NI is I + DI, NJ is J + DJ,
											tamanho(TAM),
											NI > 0, NI =< TAM,
											NJ > 0, NJ =< TAM,
											mina(NI, NJ), !,
											calcula_adj(I, J, W, L1, L2),
											N is W + 1.
%% Caso em que a casa em questão não tem mina, chamando o próximo vizinho recursivamente.
calcula_adj(I, J, N, [_|L1], [_|L2]) :- 	calcula_adj(I, J, N, L1, L2).