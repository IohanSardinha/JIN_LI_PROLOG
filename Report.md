# PLOG - Avaliação Intercalar

Grupo: JIN_LI_5

Turma: 5

Iohan Xavier Sardinha Dutra Soares (up201801011)

João Diogo Vila Franca Gonçalves (up201806162)

---
### Instalação e Execução
Abrir o SICStus Prolog e fazer consult do ficheiro "game.pl".

### O Jogo Jin_Li

Neste jogo, cada jogador possui duas carpas e dez pedras. Um par de carpas vermelho outro amarelo.

O Objetivo do jogo é fazer dez pontos, fazendo com que suas carpas tenham o máximo de contato umas com as outras.

O tabuleiro do jogo tem formato quadrado, com 7x7 casas.

Os jogadores começam com suas carpas nas extremidades do tabuleiro: Em cima as duas vermelhas uma na extrimidade esquerda e outra na direita, e da mesma forma, em baixo, as amarelas.

Os jogadores intercalam jogadas.

Em cada jogada podem se mover de duas formas:

- **Nadar**: Onde se move uma casa em qualquer direção, diagonal ou lateral, e após nadar põe uma pedra em qualquer casa vazia do tabuleiro. Se já tiver posto todas as dez pedras não se põe nenhuma.
- **Saltar**: O jogador pode saltar por cima de uma pedra adjacente, também em qualquer direção, aterrisando numa casa vazia do outro lado. As carpas só conseguem saltar uma pedra de cada vez e após fazê-lo o jogador **não** pode por uma nova pedra no tabuleiro, diferente de quando nada.

Ao final de cada jogada o jogador que se moveu ganha pontos de acordo com a quantidade de carpas adjecentes a carpa que se acabou de se mover, independente da cor do outro peixe.

O jogo acaba assim que um jogador acumular dez pontos.

---

### Lógica do Jogo:
#### Representação do Estado do Jogo   
O estado do jogo é representado por:

- Tabuleiro
  Constituido de uma lista de listas, com o estado de cada casa do tabuleiro, podendo ser:

  - _empty_ - Casa vazia
  - _red_ - Carpa vermelha
  - _yellow_ - Carpa amarela
  - _stone_ - Pedra

- Pontuação
  Valor da pontuação de cada jogador, inicialmente zero para os dois e será incrementado a cada rodada que o jogador pontuar. Através de predicados dinâmicos.
- Quantidade de Pedras de cada jogador
  Quantidade de pedras é inicialmente dez e é decrementada toda vez que um jogador coloca uma pedra no tabuleiro, através de predicados dinâmicos.
- Jogador da vez
  Representado por um 0 ou 1 equivalendo a cada jogador, 0 - vermelho e 1 - amarelo. Jogador da vez alterna entre os dois valores de rodada em rodada

Demostração dos estados do jogo, inicial, meio e final, respectivamente:

![Initial Games State](/initial.png)
![Middle Games State](/middle.png)
![Final Games State](/final.png)

> R - Carpa Vermelha;  
> Y - Carpa Amarela;  
> O - Pedra;

---

#### Visualização Do Estado do Jogo

Apos invocar o predicado play/0 para iniciar o jogo, é feito display de um menu que permite ao utilizador escolher entre os modos disponíveis.
 
![main_menu](/main_menu.png)

É feita a verificação do input do utilizador, que caso coincida com alguma opção leva o utilizador para a mesma, e caso contrario pede de novo o input dizendo que o anterior foi invalido.

![invalid_main_menu](/invalid_main_menu.png)

A vizulização do tabuleiro começa por mostrar um cabeçalho, com a cor do jogador atual e o numero das casas como referencia.

Então é chamado o predicato recursivo _printLines_ que recebe o tabuleiro e a linha a ser desenhada, vai unificar através do predicato _letter_ a letra equivalente a cada linha, para apresentar como referencia, chamará _printObjects_ e então chamará _printLines_ com a linha a ser apresentada incrementada em um.

Já _printObjects_ irá percorrer os elementos da linha e fazer a unificação entre cada objeto de representação interna de estado e o caracter que será apresentado na tela para represená-lo, através do predicato _symbol_.

Codigo responsavel pelo display do Estado de Jogo:

![Display](/display.png)

#### Lista de Jogadas Válidas
Existem, como referido em cima dois tipos de movimentos, nadar e saltar. Assim sendo temos de calcular todos os movimentos possíveis, sendo que, no caso de o movimento ser nadar a carpa apenas se pode mover para casas adjacentes e que estejam vazias. Os saltos são possíveis apenas quando existe uma pedra adjacente
á peça que se pretende mover e a casa imediatamente a seguir a pedra, na mesma direção, se encontra vazia.

#### Execução de Jogadas
Para efetuar jogadas foi implementado o predicado move(+Gamestate, +Move, -NewGameState), que recebe um Gamestate e um movimento e apos verificar que se trata de um movimento valido retorna NewGameState com a jogada já efetuada.
![move1](/move1.png)
![move2](/move2.png)
#### Final do Jogo
O jogo chega ao fim quando um dos jogadores atingir pontuação igual a 10 pontos. Assim e necessário verificar depois de cada jogada se a pontuação do jogador que a fez chegou a 10.
Com esse fim foi implementado o predicado game_over(+GameState, -Winner) que recebe o GameState apos a jogada e retorna em Winner o jogador que fez a jogada caso a sua pontuação seja maior ou igual a 10.  

![game_over1](/game_over_1.png)
![game_over2](/game_over_2.png)

