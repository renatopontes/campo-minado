:- module(gera_jogo, [posicao/2, get_tamanho/1]).
%% define um módulo gera_jogo que pode ser acessado externamente, deixando
%% posicao/2 e get_tamanho/1 como regras acessíveis ao importar o módulo em outro
%% arquivo.

:- dynamic aberto/2.
:- dynamic mina/2.
:- dynamic valor/3.
:- dynamic tamanho/1.
:- dynamic arquivo_aberto/0.
:- dynamic jogada/1.

%% Mantém os arquivos mina.pl e ambiente.pl disponíveis para consultas
:- ['mina'].
:- ['ambiente'].

%% Entrada do programa para a primeira jogada em uma posição (I,J).
%% Verifica se o arquivo jogo.pl já não está aberto. Caso não esteja, o
%% abre e começa o jogo.
posicao(I, J) :- 	not(arquivo_aberto), !,
					open('jogo.pl', write, Stream),
					asserta(jogada(1)),
					escreve_jogada(1, I, J, Stream),
					assert(arquivo_aberto),
					posicao(I, J, Stream).
%% Caso em que o arquivo jogo.pl já está aberto, sendo necessário escrever
%% somente ao fim do arquivo.
posicao(I, J) :- 	open('jogo.pl', append, Stream),
					incrementa_jogada(L),
					escreve_jogada(L, I, J, Stream),
					posicao(I, J, Stream).
%% Abre a casa escolhida passando o Stream para escrita no arquivo jogo.pl
posicao(I, J, Stream) :-	abrir_casa(I,J,Stream),
							close(Stream).

%% Para uma jogada L escreve no arquivo jogo.pl a posição escolhida
escreve_jogada(L, I, J, Stream) :- 	swritef(StrAux, '/*JOGADA %d*/\n/*posicao(%d,%d).*/\n/*AMBIENTE*/\n', [L, I, J]),
									write(Stream, StrAux).

%% Caso não seja a primeira jogada, incrementa o número da jogada e atualiza
%% o número da jogada atual.
incrementa_jogada(L) :- 	jogada(K), !,
							L is K + 1,
							asserta(jogada(L)).

%% Caso a casa (I,J) já esteja aberta, retorna TRUE
abrir_casa(I, J, _) :- 		aberto(I, J), !.
%% Caso a casa I, J tenha mina, escreve jogo_encerrado no arquivo jogo.pl e
%% para o programa.
abrir_casa(I, J, Stream) :- mina(I,J), !,
							assert(aberto(I,J)),
							escreve_resposta('jogo_encerrado', Stream),
							halt.
%% Caso a casa (I,J) que desejamos abrir tenha alguma mina adjacente, abrimos a casa
%% e escrevemos em jogo.pl as informações da casa aberta, incluindo sua coordenada
%% e o número de minas adjacentes.
abrir_casa(I, J, Stream) :- valor(I, J, K), K > 0, !,
							assert(aberto(I,J)),
							swritef(StrAux, 'valor(%d,%d,%d)', [I, J, K]),
							escreve_resposta(StrAux, Stream).

%% Caso a casa não tenha mina adjacente, escrevemos as informações da casa aberta e
%% expandimos sucessivamente seus vizinhos até encontrar um que tenha mina adjacente
%% ou chegar aos limites do tabuleiro.
abrir_casa(I, J, Stream) :- swritef(StrAux, 'valor(%d,%d,0)', [I, J]),
							escreve_resposta(StrAux, Stream),
							assert(aberto(I,J)),
							expandir_casa(I, J, Stream).
%% Escreve uma string, que pode ter os valores jogo_encerrado ou valor(I,J,K), acrescentando
%% ao final um ponto final da sintaxe do Prolog e uma quebra de linha.
escreve_resposta(String, Stream) :- atom_concat(String, '.\n', StringFile),
									write(Stream, StringFile),
									writeln(String).

%% A partir de uma casa (I,J) percorremos todos os seus vizinhos utilizando-se do vetor de
%% deslocamento.
expandir_casa(I, J, Stream) :- expandir_casa(I, J, [-1, -1, -1, 0, 1, 1, 1, 0], [-1,  0,  1, 1, 1, 0, -1, -1], Stream).
%% Quando terminarmos com o vetor de deslocamento, já teremos processado um último vizinhos
%% da casa (I,J) em questão.
expandir_casa(_, _, [], [], _) :- !.
%% Para uma casa (I,J), obtemos o seu primeiro vizinho (I+DI,J+DJ) e caso ele ainda não tenha
%% sido aberto, pedimos para que o seja voltando a regra abrir_casa a partir deste vizinho.
%% Continuamos a expandir até que não haja mais vizinhos a serem vistos.
expandir_casa(I, J, [DI|L1], [DJ|L2], Stream) :- 	NI is I + DI, NJ is J + DJ,
													tamanho(TAM),
													NI > 0, NI =< TAM,
													NJ > 0, NJ =< TAM,
													not(aberto(NI, NJ)), !,
													abrir_casa(NI, NJ, Stream),
													expandir_casa(I, J, L1, L2, Stream).
%% Caso a casa (I,J) já tenha sido aberta, simplesmente expandimos a partir do próximo vizinho.
expandir_casa(I, J, [_|L1], [_|L2], Stream) :- 	expandir_casa(I, J, L1, L2, Stream).

%% retorna o tamanho/dimensão do tabuleiro.
get_tamanho(TAM) :- tamanho(TAM).