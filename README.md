0. AUTORES
--------------------------------------------------------------------------------

	Renato Pontes Rodrigues
	Ygor Luis Mesquita Pereira da Hora


1. REQUISITOS
--------------------------------------------------------------------------------

. Python 3
. SWI-Prolog, de preferência a versão estável mais recente quando esse documento foi escrito: 7.2.3.
. Ler esse documento de forma que pelo menos 80 caracteres sejam mostrados por linha, do contrário a formatação poderá ficar incorreta em alguns lugares. De preferência, usar largura de exatamente 80 caracteres.


2. INSTRUÇÕES DE USO
--------------------------------------------------------------------------------

O script 'gera_minapl.py' (escrito em Python3) cria um arquivo de entrada 'mina.pl' a partir de um tabuleiro aleatório TAMxTAM, com um número MINAS de minas. Para rodar o script, faça:

	$ python3 gera_minapl.py TAM MINAS

No Windows pode ser necessário dar o caminho completo para o executável python.exe, se não estiver na variável PATH.

O script desenhará o tabuleiro resolvido na tela. Ex:

     1   2   3   4
    ---------------
 1 |   |   | 1 | 1 |	Para salvar o desenho, use redirecionamento. Ex:
   |--- --- --- ---|
 2 |   |   | 1 | # |		$ python3 gera_minapl.py 4 3 > tabuleiro
   |--- --- --- ---|
 3 | 1 | 1 | 3 | 2 |
   |--- --- --- ---|
 4 | 1 | # | 2 | # |
    ---------------

Também escreverá no arquivo 'mina.pl' as regras Prolog correspondentes as posições com mina, e também uma regra que diz o tamanho do tabuleiro. Ex:

tamanho(4).
mina(2,4).
mina(4,2).
mina(4,4).

O primeiro programa do enunciado se chama 'gera_ambiente.pl'. Ao executar, esse programa gera o resto da configuração do tabuleiro e grava os dados no arquivo 'ambiente.pl', da forma especificada no enunciado. Ex:

valor(1,1,0).
valor(1,2,0).
valor(1,3,1).
valor(1,4,1).
valor(2,1,0).
...

Para executar, abra o arquivo 'gera_ambiente.pl' no Prolog e faça:

	?- inicio.

O programa simplesmente termina e deverá ter sido criado o arquivo 'ambiente.pl' no mesmo diretório.
Depois disso, basta abrir o arquivo 'agente.pl' (Programa 3 do enunciado) no Prolog e iniciá-lo com:

	?- inicio.

O agente utiliza um outro arquivo, 'gera_jogo.pl' (Programa 2), para fazer as consultas. Para cada consulta do agente, 'gera_jogo.pl' escreve na tela e no arquivo 'jogo.pl' as casas que foram reveladas. Ex:

. jogo.pl:
	/*JOGADA 1*/
	/*posicao(3,4).*/
	/*AMBIENTE*/
	valor(3,4,2).
	/*JOGADA 2*/
	/*posicao(2,2).*/
	/*AMBIENTE*/
	valor(2,2,0).
	valor(1,1,0).
	valor(1,2,0).
	...

. agente no console do Prolog:
	?-	inicio.
		valor(3,4,2)
		valor(2,2,0).
		valor(1,1,0).
		valor(1,2,0).
		...

O arquivo 'jogo.pl' contém um comentário no início de cada jogada, com número da jogada e posição que foi aberta, antes de mostrar as casas que foram reveladas com aquela jogada. Na tela, são impressas apenas as casas que são reveladas.
Quando se clica em uma mina, é impresso na tela 'jogo encerrado' e o programa termina. Em 'jogo.pl', aparece 'jogo_encerrado.', com sintaxe válida do Prolog.
Quando o agente ganha o jogo, é impresso na tela 'Ganhou :D' e o programa termina, mas nada é escrito no arquivo.


3. PROGRAMA 1: gera_ambiente.pl
--------------------------------------------------------------------------------

COMO RODAR: Utilizar a regra 'inicio' no terminal do Prolog.

ENTRADA: Arquivo 'mina.pl'. Não precisa ser carregado explicitamente, mas precisa existir no mesmo diretório.

SAÍDA: Arquivo 'ambiente.pl'. Ocorre sobrescrita se já existir.

--------------------------------------------------------------------------------

Este programa anda pelo tabuleiro a partir da posição (1,1) até a posição (TAM, TAM), da esquerda para a direita e de cima para baixo, fazendo a seguinte verificação para cada casa:
	1. Se a casa (I, J) não tem mina (consultando o arquivo 'mina.pl'), é calculado o número K de minas adjacentes e é adicionada uma linha ao final do arquivo 'ambiente.pl', na forma 'valor(I,J,K).'.
	2. Se a casa (I, J) tem mina, não fazemos nada, apenas vamos para a próxima casa.

Para percorrer os vizinhos de uma casa (I,J), utilizamos vetores de deslocamento. É mais fácil mostrar com um exemplo:

+--------+--------+--------+
|        |        |        |
| -1, -1 | -1, 0  | -1, 1  |	A posição absoluta do vizinho do canto superior
|        |        |        |	direito é (Iur, Jur). Se fizermos um vetor de
+--------+--------+--------+	(I,J) até (Iur, Jur), teremos:
|        |        |        |		(Iur, Jur) - (I, J) = (-1, 1)
| 0, -1  |  I, J  | 0, 1   |	Que é a posição relativa do vizinho a partir de
|        |        |        |	(I, J).
+--------+--------+--------+
|        |        |        |
| 1, -1  |  1, 0  | 1, 1   |
|        |        |        |
+--------+--------+--------+

A partir de uma casa (I, J) qualquer, o deslocamento de (I, J) até um certo vizinho é constante. Criamos vetores de deslocamento para cada componente:
Vdi = [-1, -1, -1, 0, 1, 1, 1, 0]
Vdj = [-1,  0,  1, 1, 1, 0, -1, -1]

Tomando os elementos de índice 0 de cada vetor, temos (-1, -1), o deslocamento para o vizinho do canto superior esquerdo a partir de (I, J).
Tomando os elementos de índice 1 de cada vetor, temos (-1, 0), o deslocamento para o vizinho superior a partir de (I, J), e assim sucessivamente, em sentido horário a partir do vizinho do canto esquerdo superior.
Sabemos que percorremos todos os vizinhos quando terminamos de percorrer os vetores Vdi e Vdj.

Se um vizinho tem coordenadas válidas e tem mina, adicionamos 1 a contagem de minas adjacentes dos próximos vizinhos. Do contrário, apenas retornamos a contagem de minas a partir do vizinho seguinte.

Esta estratégia para percorrer vizinhos foi usada várias vezes em todo o código, por isso a explicação longa.


4. PROGRAMA 2: gera_jogo.pl
--------------------------------------------------------------------------------

Este arquivo é um módulo que oferece duas regras públicas:
. posicao/2: posicao(I,J) abre a casa (I,J).
. get_tamanho/1: get_tamanho(TAM) é verdadeiro se TAM é o tamanho do tabuleiro.

COMO RODAR: Utilizar a regra 'posicao/2' no terminal do Prolog, onde o primeiro argumento é a coordenada I e o segundo é a coordenada J. Esse programa é utilizado para consultas do agente, e não precisa ser executado diretamente.

ENTRADA: Arquivos 'mina.pl' e 'ambiente.pl'. Não precisam ser carregados explicitamente, mas precisam existir no mesmo diretório.

SAÍDA: Arquivo 'jogo.pl'. Ocorre sobrescrita se já existir. Na tela são impressos os mesmos dados do arquivo, mas sem os comentários.

--------------------------------------------------------------------------------

Na primeira consulta, o arquivo 'jogo.pl' é criado, ou apagado se já existir.
Nas consultas seguintes o arquivo é aberto no modo 'append'. Isso é feito com uma regra dinâmica 'arquivo_aberto', que só existe depois que o arquivo foi aberto em alguma instância do programa pela primeira vez.

Dada uma consulta posicao(I,J), o programa faz as seguintes verificações para tentar abrir a casa (I,J):
	1. Se a casa já está aberta, retorne true.
	2. Se a casa contém mina, escreva a mensagem 'jogo encerrado' na saída e em seguida termine o jogo.
	3. Se a casa não contém mina, está fechada e tem valor maior que 0, a mensagem 'valor(I,J,K)' é adiciona a saída e é marcada como aberta utilizando a regra dinâmica 'aberto/2'.
	4. Se a casa não contém mina, está fechada e tem valor 0, então abra a casa como no caso (3), mas também aplique a regra posicao/2 a cada vizinho desta casa.

OBS2: Note que uma casa aberta indiretamente por várias consultas diferentes aparece na saída uma única vez: quando tem seu valor descoberto pela primeira vez (verificado por meio da regra dinâmica 'aberto/2').


5. PROGRAMA 3: agente.pl
--------------------------------------------------------------------------------

COMO RODAR: Utilizar a regra 'inicio' no terminal do Prolog.

ENTRADA: Arquivo 'jogo.pl'. Não precisa ser carregado explicitamente, e 'jogo.pl' não precisa existir antes do programa executar.

SAÍDA: Escreve na tela 'Ganhou :D' se o agente vencer. Outras mensagens na tela e no arquivo 'jogo.pl' são decorrentes das consultas feitas ao módulo 'gera_jogo.pl'.

--------------------------------------------------------------------------------

O agente utiliza a interface pública do módulo 'gera_jogo' e o conhecimento acumulado durante o jogo (em 'jogo.pl') para tentar ganhar a partida, marcando com flag casas que ele sabe que possuem mina e abrindo casas que ele julga serem seguras.

Inicialmente, 'jogo.pl' está vazio, não existe ou está sujo com um jogo anterior. O agente gera uma posição aleatória válida e tenta abrir essa casa com 'posicao/2' (que cria ou limpa o conteúdo de 'jogo.pl').

Em seguida o agente entra em um loop infinito, onde ele executa os seguintes passos:
	1. Verifica se ganhou o jogo. Se ganhou, imprime na tela 'Ganhou :D' e termina o programa.
	2. Se não ganhou, gera a fronteira(*) e tenta jogar de forma lógica (só abre ou põe flag numa casa se tiver certeza) enquanto puder.
	3. Quando o passo (2) não consegue alterar a fronteira, o agente utiliza uma heurística para abrir *uma* casa. Em seguida volta ao passo (1).

O loop também pode ser interrompido indiretamente pelo módulo 'gera_jogo', quando uma casa que possui mina é encontrada. Nesse caso o programa imprime na tela 'jogo encerrado' e termina.

(*) Chamamos de "fronteira" uma lista com todas as casas abertas de onde podem ser extraídas informações de que casas devem ser abertas no futuro. As casas na fronteira satisfazem as seguintes restrições, ao mesmo tempo:
	1. Possui valor maior que 0:
		Existe informação sobre a existência de minas nas adjacências, portanto essa casa é cadidata a entrar na fronteira. Uma casa com valor 0 é inútil porque o módulo 'gera_jogo' já terá aberto suas adjacências.

	2. Possui pelo menos uma casa vizinha fechada:
		Se existem casas fechadas em volta, então talvez ainda seja necessário usar o valor dessa casa para descobrir que casas em volta devem ser abertas ou marcadas com flag.

	3. Pelo menos uma das casas vizinhas fechadas não tem flag.
		Se todas as casas fechadas em volta estão marcadas com flag, então todas as casas vizinhas que deveriam ser abertas foram de fato abertas, e não há mais nenhuma informação que se possa tirar dessa casa.

As subseções a seguir descrevem como o agente joga logicamente e como ele joga heuristicamente.


5.1 JOGADAS LÓGICAS: jogar_logica/2, jogar_logica/3
--------------------------------------------------------------------------------

A regra 'jogar_logica/3' é outro loop infinito. O loop é interrompido quando esta regra gera a mesma fronteira da iteração anterior.

Entre uma iteração e outra, a fronteira é recalculada.
A cada iteração, o agente percorre a fronteira e, para cada casa (I,J) com valor K, executa as seguintes verificações:
	1. Se o número de vizinhos com flag for igual a K, então as localizações de todas as minas adjacentes são conhecidas, e portanto é seguro abrir qualquer casa vizinha que não esteja com flag. A regra 'abre_vizinhos/2' abre as casas fechadas sem flag em torno de (I,J).
	2. Se o número de vizinhos fechados é igual a K, então todas essas casas serão marcadas com flag (se já não estiverem marcadas). A regra 'flag_vizinhos/2' é responsável por marcar os vizinhos fechados de (I,J).

No caso em que a fronteira é vazia, só é retornado true.

Quando essas regras não são mais suficientes para modificar a fronteira, fazemos uma jogada utilizando heurística.


5.2 JOGADA HEURÍSTICA: jogar_heuris/2, jogar_heuris/3
--------------------------------------------------------------------------------

Utilizamos a probabilidade local de cada casa fechada próxima a fronteira para estimar a melhor casa a ser aberta. É um cálculo incorreto porque a probabilidade de uma casa fechada ter mina depende de mais do que apenas uma casa aberta adjacente. Por isso chamamos de heurística. É mais fácil explicar com um exemplo:

     1   2   3   4
    ---------------
 1 |   |   | 1 | - |	Nesse exemplo, olhando apenas a casa (1,3), dizemos que
   |--- --- --- ---|	a probabilidade p(1,4) = p(2,4) = 0.50.
 2 |   |   | 1 | - |	Olhando a casa (2,3), dizemos que p(1,4) = p(2,4) =
   |--- --- --- ---|	p(3,4) = 0.33.
 3 | 1 | 1 | 3 | - |	Em casos de conflito, ficamos com a probabilidade mais
   |--- --- --- ---|	pessimista. Logo, p(1,4) = p(2,4) = 0.50 e
 4 | - | - | - | - |	p(3,4) = 0.33.
    ---------------

Seja (I,J) uma casa da fronteira com valor K, Nflag casas adjacentes com flag e Nfech casas adjacentes fechadas. Para cada vizinho v de (I,J) dizemos que
	p(v) = (K - Nflag) / (Nfech - Nflag)
Colocando de outra forma, é o número de minas adjacentes que eu não sei onde estão dividido pelo número de casas adjacentes que eu posso abrir.
Como dito no exemplo, em caso de conflitos fica a maior probabilidade.

Seguem os passos executados pelo agente:
	1. O agente cria uma lista PL a partir da fronteira com as casas candidatas a serem abertas, onde cada item da lista tem a forma (P, I, J), onde P é a "probabilidade" da casa (I,J) ter mina.
	2. A menor probabilidade encontrada Pmin é determinada e é criada uma nova lista MinPL onde só aparecem as casas de PL onde P = Pmin.
	3. Uma casa aleatória de MinPL é selecionada e aberta com 'posicao/2'.
Se PL for vazio, apenas retornamos true.

Se o agente não perder nessa jogada, pode ser possível determinar as próximas jogadas a partir do método anterior. A fronteira é recalculada e voltamos a jogar logicamente.