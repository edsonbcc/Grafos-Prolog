
/* via terminal
?- adjacentes_lista(curitiba, L).
?- profundidade(curitiba, rio_branco, [], L).
?- largura(curitiba, rio_branco, L).
?- distancia_lista(curitiba, sao_paulo, L).
?- vert_distancia_lista(curitiba, 2, L).
*/


ligado(porto_alegre, florianopolis).
ligado(florianopolis, curitiba).
ligado(curitiba, sao_paulo).
ligado(curitiba, campo_grande).
ligado(sao_paulo, campo_grande).
ligado(sao_paulo, belo_horizonte).
ligado(sao_paulo, rio_de_janeiro).

ligado(campo_grande, cuiaba).
ligado(campo_grande, goiania).
ligado(campo_grande, belo_horizonte).
ligado(belo_horizonte, rio_de_janeiro).

ligado(belo_horizonte, vitoria).
ligado(belo_horizonte, goiania).
ligado(belo_horizonte, salvador).

ligado(rio_de_janeiro, vitoria).
ligado(cuiaba, goiania).
ligado(cuiaba, porto_velho).
ligado(cuiaba, manaus).
ligado(cuiaba, belem).
ligado(cuiaba, palmas).
ligado(goiania, palmas).
ligado(goiania, salvador).
ligado(vitoria, salvador).
ligado(salvador, palmas).
ligado(salvador, teresina).
ligado(salvador, recife).
ligado(salvador, aracaju).
ligado(salvador, maceio).
ligado(porto_velho, rio_branco).
ligado(porto_velho, manaus).
ligado(manaus, rio_branco).
ligado(manaus, belem).
ligado(manaus, boa_vista).
ligado(belem, macapa).
ligado(belem, boa_vista).
ligado(belem, sao_luis).
ligado(belem, palmas).
ligado(palmas, sao_luis).
ligado(palmas, teresina).
ligado(teresina, sao_luis).
ligado(teresina, fortaleza).
ligado(teresina, recife).
ligado(recife, maceio).
ligado(recife, fortaleza).
ligado(recife, joao_pessoa).
ligado(aracaju, maceio).
ligado(fortaleza, natal).
ligado(fortaleza, joao_pessoa).
ligado(joao_pessoa, natal).

/*
% para testes
ligado(0,1).
ligado(1,4).
ligado(1,5).
ligado(0,2).
ligado(2, 6).
ligado(6, 9).
ligado(0,3).
ligado(3,7).
ligado(7,10).
ligado(3,8).
ligado(5, 2).
ligado(2, 3).
%ligado(9,10).
*/

menu() :-
       nl,
       writeln('Digite a opcao'),
       writeln('1- Verificar adjacentes de um determinado vertice'),
       writeln('2- Busca em profundidade'),
       writeln('3- Busca em largura'),
       writeln('4- Nos que estao a uma distancia D de um no N'),
       writeln('5- Distancias possiveis entre os vertice X-Y'),
       writeln('0- Sair'),
       write('Escolha: '),
       read(Escolha),
       opcao(Escolha),
       0 \= Escolha, % se 0 escolha for diferente de zero o programa chama ele de novo recursivamente
       menu().


opcao(0) :-
	 nl, writeln(' Saindo...'), !.

opcao(1) :-
	nl,writeln(' Predicado Adjacentes'),
	write('Digite a origem: '),
	read(Origem),
	adjacentes_lista(Origem, L),
	imprime(L), !.

opcao(2) :-
         nl, writeln('Busca profundidade...'),
	 write('Digite a origem: '),
	 read(Origem),
	 write('Digite a destino: '),
	 read(Destino),
	 profundidade(Origem, Destino, [], R), %lista é retornada invertida

	 inverter(R, R1), % predicado para inverter a lista
	 % imprime(R), % print R original (invertida)
	 nl, writeln('Caminho: '),
	 imprime(R1), writeln(' '),

	 tamanho(R1, T), % pega a distancia (tamanho) do caminho
	 Tamanho is T -1,
	 writeln('Tamanho do caminho eh: ':Tamanho), !. % print R1 (correta).




opcao(3) :-
	 nl,writeln(' Busca largura...'),
	 write('Digite a origem: '),
	 read(Origem),
	 write('Digite a destino: '),
	 read(Destino),
	 largura(Origem, Destino, L),
	 imprime(L),!.



opcao(4) :-
	 nl,writeln('Vertices que um Vertice Origem pode alcancar com distancia D'),
	 write('Digite a origem: '),
	 read(Origem),
	 write('Digite a distancia '),
	 read(D),
	 vert_distancia_lista(Origem, D, AuxL),
	 retira_repetidos(AuxL, L),
	 imprime(L),!.

opcao(5) :-
	 nl,writeln('Distancias possiveis entre os vertices X-Y...'),
	 write('Digite a origem: '),
	 read(Origem),
	 write('Digite a destino: '),
	 read(Destino),

	 distancia_lista(Origem, Destino, L),
	 imprime(L),!.


opcao(_) :- % caso usuario digite qualquer outra coisa
	nl, writeln('Fim do predicado'), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adjacentes(X,Y):-
	ligado(X,Y).

adjacentes(X,Y):-
	ligado(Y,X).

%coloca adjacentes de um vertice em uma lista
adjacentes_lista(Origem, Lista_saida) :-
	findall(Adj, adjacentes(Origem, Adj), Lista_saida).

/*
% para interface
adjacentes(Origem) :-
	adjacentes(Origem, Aux_Adj),
	writeln('Adjacente ':Aux_Adj), fail.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% condicao de parada
profundidade(Destino,Destino,V, [Destino|V]) :- !.

% ?- profundidade(Origem, Destino, [], R).
profundidade(Origem, Destino, V, Resultado) :-
	\+ member(Origem,V),
	adjacentes(Origem,A),
	profundidade(A,Destino, [Origem|V], Resultado).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%interface - coloca primeiro origem na fila e chama predicado
largura(Origem, Destino, R) :-
	insere_inicio(Origem, [], L), % insere origem em L (que tera saida)
	insere_inicio(Origem, [], Visitados), % insere origem em visitados
	largura(Destino, L, R1, Visitados), % chama predicado
	retira_repetidos(R1, R2),
	inverter(R2, R). % inverte resultado da busca em largura

largura(Destino, [Destino|_], [Destino|Visitados], Visitados) :-  !.

largura(Destino, [X|Y], R, V1) :-
	adjacentes_lista(X, LA),
	conc(Y, LA, Saida), % concatena corpo da fila com adjacentes ( e retira X)
	insere_inicio(X, V1, NewV), % atualiza lista de visitados
	remove_visitados(Saida, NewV, SaidaLimpa), % remove da fila os vertices ja visitados
	largura(Destino, SaidaLimpa, R, NewV).


insere_inicio(Elemento, L, [Elemento|L]) :- !.

%remove visitados da fila
remove_visitados([], _, []):- !.

remove_visitados([X|Y], L, [X|Z]) :-
	\+ member(X,L),
	remove_visitados(Y, L, Z), !.

remove_visitados([_|Y], L, Z) :-
	remove_visitados(Y, L, Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%terminal - ?- vert_distancia_lista(curitiba, 2, L).
vert_distancia_lista(Origem, Distancia, L) :-
	setof(Vertice, vert_distancia(Origem, Distancia, Vertice), AuxL),
	retira_repetidos(AuxL, L).

% so para criar a lista de vertices e passar como parametros
vert_distancia(Origem, Distancia, Destino) :-
	%write('L = [ '),
	vertices(L_vertices),
	vd(Origem,Distancia, L_vertices, Destino).


% condicao de parada (fim da lista de vertices)
vd(_, _, [], _):- !.

% faz uma busca para cada vertice e verifica se a distancia é a mesma do
% caminho gerado
vd(Origem, Distancia, [X|Y], Destino) :-
	distancia_t2(Origem, X, Distancia, Destino),
	vd(Origem,Distancia ,Y, Destino).

vd(Origem, Distancia, [_|Y], Destino) :-
	vd(Origem, Distancia, Y, Destino).

% faz todos os caminhos possiveis de um vertice X a Y e calcula tamanho.
distancia_t2(Origem, Destino, Distancia, Destino) :-
	 profundidade(Origem, Destino, [], R), %lista é retornada invertida
	 tamanho(R, T), % pega a distancia (tamanho) do caminho
	 Tamanho is T -1,
	 Tamanho =:= Distancia, !.
	% Destino_aux is Destino.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% faz todos os caminhos possiveis de um vertice X a Y e calcula tamanho.
distancia(Origem, Destino, Tamanho) :-
	 nl, write('Distancias Possiveis: '),

	 profundidade(Origem, Destino, [], R), %lista é retornada invertida

	 tamanho(R, T), % pega a distancia (tamanho) do caminho
	 Tamanho is T -1.

%terminal - ?- distancia_lista(curitiba, sao_paulo, L).
distancia_lista(Origem, Destino, L) :-
	setof(Tamanho, distancia(Origem, Destino, Tamanho), AuxL),
	retira_repetidos(AuxL, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% coloca todos os vertices do grafo em uma lista
vertices(L1) :-
	findall(X, ligado(X, _); ligado(_, X), L),
	%imprime(L), nl,
	retira_repetidos(L, L1).
	%imprime(L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% retira repetidos de uma lista
retira_repetidos([], []).
retira_repetidos([Cabeca|Corpo], [Cabeca|Saida]) :-
	not(member(Cabeca, Corpo)),
	retira_repetidos(Corpo, Saida), !.

retira_repetidos([_|Corpo], Saida) :-
	retira_repetidos(Corpo, Saida).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



tamanho([], 0).
tamanho([_|Y], T) :-
	tamanho(Y,T1),
	T is T1 +1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


imprime(L) :-
	nl, write('L = [ '),
	print(L),
	nl,nl.

print([]) :-
	write(']').

print([X|Y]) :-
	write(X), tab(2),
	print(Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


inverter([],[]).
inverter([X|Y],Z):-
	inverter(Y,Y1),
	conc(Y1,[X],Z).


conc([],L,L).
conc([X|L1],L2,[X|L3]):-
	conc(L1,L2,L3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




















