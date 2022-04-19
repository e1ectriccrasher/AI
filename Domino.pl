/*-------------------- Instructions --------------------*/
% You can start the game using:
% startGame.


% Still implementing:

% Need to finish! This method returns the possible pieces in the enemy hand
% This method is suposed to get a list containing the possible domino tiles that can be played by us.
% write("What is the enemy doing (d: draw| p: playing):"),
% /*-------------------- Board & Game Engine --------------------*/

% Main predicate:
startGame:- 
	nb_setval(fichasOp, [[0,0],[0,1],[0,2],[0,3],[0,4],[0,5],[0,6],[1,1],[1,2],[1,3],[1,4],[1,5],[1,6],[2,2],[2,3],[2,4],[2,5],[2,6],[3,3],[3,4],[3,5],[3,6],[4,4],[4,5],[4,6],[5,5],[5,6],[6,6]]),
	nb_setval(numFichas, 7),
	nb_setval(numFichasOp, 7),
	nb_setval(tablero, []),
	write("Enter your tiles (Format [[A,B],[C,D]...]: "), nl,
	read(A), nl,
	nb_setval(fichas, A),
	nb_getval(fichasOp, Op),
	deleteList(A, Op, NOp),
	nb_setval(fichasOp, NOp),
	write("You start the game? (y/n) "), nl,
	read(Prim),
	firstStep(Prim, T),
	play(T).

% firstStep(i, o): Makes first move and continue game 

% case n: Opponent throw first tile
%  Put on the layout
%  Changes list from opponent(handOp), possible pieces on the hand of opponent
% decrease (numFichasOp)
firstStep(n, T):-
	write("Enter tile from opponent: "), nl,
	read(F),
	nb_setval(tablero, [F,F]),
	nb_setval(numFichasOp, 6),
	nb_getval(fichasOp, FichasOp),
	deleteTile(F, FichasOp, NuevasFichas),
	nb_setval(fichasOp, NuevasFichas),
	T is 1.

% case y: we put the first tile
% Put on the layout
% changes tile on hand(fichas)
% decrease(numFichas)
firstStep(y, T):-
	write("Enter first tile: "), nl,
	read(F),
	nb_setval(tablero, [F,F]),
	nb_setval(numFichas, 6),
	nb_getval(fichas, Fichas),
	deleteTile(F, Fichas, NuevasFichas),
	nb_setval(fichas, NuevasFichas),
	T is 0.

% play(i): method to make all the moves ours/opponent
% case 0: opponent turn
% checks if he won
% Print layout
% Ask if the opponent took tile
% Ask if opponent throws on head or tail
% Llama al predicado opponentTurn(i, i)
% Llama al predicado play(1) para cambiar de turno
play(0):-
	nb_getval(numFichas, Num),
	((Num < 1, write("YOU WON!!!!!!!! :)"), nl, !);
	(nb_getval(tablero, Tab),
	length(Tab, Len),
	write("LAYOUT: "), nl,
	write("----"), printLine(Len), write("----"), nl,
	write("|"), write(Tab), write("|"), nl,
	write("----"), printLine(Len), write("----"), nl,
	nl, write("How many does the opponent take?"), nl,
	read(N),
	write("-1: Opponent thown on the head"),nl,
	write(" 0: if it did not throw"),nl,
	write(" 1: throws on tail"), nl,
	read(U),
	opponentTurn(N, U), nl,
	play(1))).


play(1):-
	nb_getval(numFichasOp, Num),
	write("The opponent has "), write(Num), write(" tiles"), nl,
	((Num < 1, write("YOU LOST!!!!!!!! :("), nl, !);
	(nb_getval(tablero, Tab),
	length(Tab, Len),
	write("LAYOUT: "), nl,
	write("--"), printLine(Len), write("--"), nl,
	write("|"), write(Tab), write("|"), nl,
	write("--"), printLine(Len), write("--"), nl,
	nb_getval(fichas, Fichas),
	nl, write("YOUR HAND: "), write(Fichas), nl,
	makeMove(), nl,
	play(0))).


opponentTurn(N, 0):-
	checkTie(),
	nb_getval(numFichasOp, M),
	NuevoNum is N + M,
	nb_setval(numFichasOp, NuevoNum),
	nb_setval(empate, 1).


opponentTurn(N, V):-
	write("Put opponent tile: "), nl,
	read(F),
	changeTile(F, V, NF),
	settle(V, NF),
	nb_getval(fichasOp, Op),
	deleteTile(NF, Op, NOp),
	nb_setval(fichasOp, NOp),
	nb_getval(numFichasOp, M),
	NuevoNum is (N + M - 1),
	nb_setval(empate, 0),
	nb_setval(numFichasOp, NuevoNum).

changeTile(F, 1, NF):-
	head(F, HF), last(F, TF),
	nb_getval(tablero, Tab),
	last(Tab, Ult), last(Ult, TT),
	((HF =:= TT, NF = F);
		(NF = [TF, HF])).

changeTile(F, -1, NF):-
	head(F, HF), last(F, TF),
	nb_getval(tablero, Tab),
	head(Tab, Prim), head(Prim, HT),
	((TF =:= HT, NF = F);
		(NF =[TF, HF])).

% settle(i, i): adjust la ficha la cabeza o cola del tablero 

% Caso -1: adjust la ficha (F) en la cabeza del tablero
settle(-1, F):- 
	nb_getval(tablero, Tab),
	nb_setval(tablero, [F|Tab]).

% Caso 1: adjust la ficha (F) en la cola del tablero
settle(1, F):- 
	nb_getval(tablero, Tab),
	union(Tab,[F], NT),
	nb_setval(tablero, NT).

% Caso 2: Busca dónde se puede settle la ficha (F) y llama a settle() según corresponda
settle(2, F):-
	nb_getval(tablero, [[HT|_]|B]),
	head(F, HF), last(F, TF), % Head Ficha y Tail Ficha
	last(B, UF), last(UF, TT), % ultima ficha y Tail de la ultima ficha
	(
		(HF =:= HT, settle(-1,[TF,HF]));
		(TF =:= HT, settle(-1, F));
		(HF =:= TT, settle(1, F));
		(TF =:= TT, settle(1, [TF,HF])),!).



makeMove:-
	nb_getval(fichas, Fichas),
	nb_getval(tablero, [[A1|_]|B]),
	nb_getval(numFichas, NumFichas),
	NuevoNum is NumFichas - 1,
	find(Fichas, A1, [], D), % D es la lista de las fichas que se pueden settle en la cabeza
	last(B, L),
	last(L, L1),
	find(Fichas, L1, D, PF), % PF es posibles fichas
	((PF == [], 
		takeNewTile(Fichas, NumFichas));
	(length(PF, 1),
		write("Possible moves: "), write(PF), nl,
	 	head(PF, F), 
		deleteTile(F, Fichas, NuevasFichas), 
	 	nb_setval(fichas, NuevasFichas), 
	 	settle(2, F), 
	 	nb_setval(empate, 0),
	 	nb_setval(numFichas, NuevoNum),
	 	nl, write("Best Move: "), write(F), nl);
	(write("Possibles moves: "), write(PF), nl,
		nb_getval(tablero, Tablero),
		% nb_getval(numFichas, Num),
		% nb_getval(numFichasOp, NumOp),
		% Profundidad is Num + NumOp,
		nb_getval(fichasOp, FichasOp),
		alphabeta(Tablero, 3, FichasOp, Fichas, Fichas, 1, -10000,  10000, MejorTiro, Valor), 
		write("Valor de la rama: "), write(Valor), nl,
		nl, write("Best move: "), write(MejorTiro), nl,
		deleteTile(MejorTiro, Fichas, NuevasFichas), nb_setval(fichas, NuevasFichas),
		nb_setval(empate, 0),
		nb_setval(numFichas, NuevoNum),
		settle(2, MejorTiro),!
		)).



takeNewTile(A, L):-
	% write("Escribe 1 si hay fichas para takeNewTile o 0 si ya se acabaron las fichas"), nl,
	% read(Valor),
	nb_getval(numFichasOp, NumFichasOp),
	nb_getval(fichasOp, FichasOp),
	length(FichasOp, Len),
	Valor is Len - NumFichasOp,
	((Valor =:= 0, write("PASS"), nl,
	nb_getval(empate, Empate),
	checkTie(Empate));
	(write("Put new tile: "), nl,
	read(F),
	nb_setval(fichas, [F|A]),
	nb_getval(fichasOp, Op),
	deleteTile(F, Op, NOp),
	nb_setval(fichasOp, NOp),
	LN is (L + 1),
	nb_setval(numFichas, LN),
	nl, write("You have "), write(LN), write(" tiles"), nl,
	makeMove())).

% Comprueba si el jugador anterior pasó y si falla si sí pasó
checkTie(0):-
	nb_setval(empate, 1),
	play(0).

checkTie(1):-
	write("TIE!!!!"), nl, fail.

% ----------------------------------------------------- MINIMAX Y FUNCIÓN HEURÍSTICA ------------------------------------------------------------------------------------


putTile(Fichas, Tiro, Tablero, NuevasFichas, NuevoTablero):-
	completeLayout(Tiro, Tablero, NuevoTablero),
	deleteTile(Tiro, Fichas, NuevasFichas).


completeLayout(Tiro, Tablero, NuevoTablero):-
	nb_setval(tab, Tablero),
	nb_getval(tab, [[HT|_]|B]),
	head(Tiro, HF), last(Tiro, TF),
	last(B, UF), last(UF, TT), 
	((HF =:= HT, adjust(-1,[TF,HF], Tablero, NuevoTablero));
	(TF =:= HT, adjust(-1, Tiro, Tablero, NuevoTablero));
	(HF =:= TT, adjust(1, Tiro, Tablero, NuevoTablero));
	(TF =:= TT, adjust(1, [TF,HF], Tablero, NuevoTablero)), !).


adjust(-1, Tiro, Tablero, NuevoTablero):- 
	union([Tiro], Tablero, NuevoTablero).


adjust(1, Tiro, Tablero, NuevoTablero):- 
	union(Tablero, [Tiro], NuevoTablero). 


alphabeta(_, _, FichasOcultas, [], _, _, _, _, _, Valor):-
	length(FichasOcultas, N1),
	Valor is N1, !.


alphabeta(MaxMin, _, FichasOcultas, MisFichas, [], _, _, _, _, Valor):-
	length(FichasOcultas, N1), length(MisFichas, N2),
	Valor is (N1 - N2) * (-MaxMin), !.


alphabeta(_, 0, FichasOcultas, MisFichas, _, _, _, _, _, Valor):-
	length(FichasOcultas, N1), length(MisFichas, N2), 
	Valor is (N1 - N2), !.


alphabeta(_, _, [], MisFichas, _, _, _, _, _, Valor):-
	length(MisFichas, N2),
	Valor is -N2, !.


alphabeta(Tablero, Profundidad, FichasOcultas, MisFichas, Buscar, MaxMin, Alfa, Beta, Movimiento, Valor):-
	Profundidad > 0,
	nb_setval(tabProv, Tablero),
	nb_getval(tabProv, [[A1|_]|B]),
	find(Buscar, A1, [], D), 
	last(B, L),
	last(L, L1),
	find(Buscar, L1, D, Posibilidades),
	Alfa1 is -Beta,
	Beta1 is -Alfa,	
	NuevaP is Profundidad - 1,
	move(MaxMin, Posibilidades, Tablero, FichasOcultas, MisFichas, Buscar, NuevaP, Alfa1, Beta1, _, (Movimiento, Valor)), !.


move(_, [], _, _, _, _, _, Alfa, _, Tiro, (Tiro, Alfa)).

move(MaxMin, [Tiro | Restantes], Tablero, FichasOcultas, MisFichas, Buscar, Profundidad, Alfa, Beta, Record, MejorTiro):-
	putTile(Buscar, Tiro, Tablero, NuevasFichas, NuevoTablero),
	changePlayer(MaxMin, OtroMaxMin),
	putNewTile(Tiro, Restantes, OtroMaxMin, NuevoTablero, Profundidad, FichasOcultas, MisFichas, NuevasFichas, Alfa, Beta, Record, MejorTiro, _), !.


putNewTile(Tiro, Restantes, 1, Tablero, Profundidad, _, MisFichas, NuevasFichas, Alfa, Beta, Record, MejorTiro, Valor):-
	alphabeta(Tablero, Profundidad, NuevasFichas, MisFichas, MisFichas, 1, Alfa, Beta, MejorTiro, Valor),
	Valor1 is -Valor,
	poda(1, Tiro, Valor1, Profundidad, Alfa, Beta, Restantes, Tablero, NuevasFichas, MisFichas, MisFichas, Record, MejorTiro).


putNewTile(Tiro, Restantes, -1, Tablero, Profundidad, FichasOcultas, _, NuevasFichas, Alfa, Beta, Record, MejorTiro, Valor):-
	alphabeta(Tablero, Profundidad, FichasOcultas, NuevasFichas, FichasOcultas, -1, Alfa, Beta, MejorTiro, Valor),
	Valor1 is -Valor,
	poda(-1, Tiro, Valor1, Profundidad, Alfa, Beta, Restantes, Tablero, FichasOcultas, NuevasFichas, FichasOcultas, Record, MejorTiro).


poda(_, Tiro, Valor, _, _, Beta , _ , _, _, _, _, _, (Tiro, Valor)) :- 
   Valor >= Beta, !.


poda(MaxMin, Tiro, Valor, Profundidad, Alfa, Beta, Restantes, Tablero, FichasOcultas, MisFichas, Buscar, _, MejorTiro) :- 
   Alfa < Valor, Valor < Beta, !, 
   move(MaxMin, Restantes, Tablero, FichasOcultas, MisFichas, Buscar, Profundidad, Valor, Beta, Tiro, MejorTiro).


poda(MaxMin, _, Valor, Profundidad, Alfa, Beta, Restantes, Tablero, FichasOcultas, MisFichas, Buscar, Record, MejorTiro) :- 
   Valor =< Alfa, !, 
   move(MaxMin, Restantes, Tablero, FichasOcultas, MisFichas, Buscar, Profundidad, Alfa, Beta, Record, MejorTiro).


changePlayer(1, -1).
changePlayer(-1, 1).

% ----------------------------------------------------- AUXILIAR METHODS------------------------------------------------------------------------------------

% find(i, i, i, o): auxiliar for list
% Find the value that are equal to head or tile(Val)

% When list is empty
find([], _, PF, PF):- !.
find([[A|B]|F], Val, C, PF):-
	((A =:= Val; B =:= Val),
	union(C, [[A|B]], N),
	find(F, Val, N, PF));
	find(F, Val, C, PF).

% deleteTile(i, i, o): auxiliar for list
% % F: element we want to delete
% % L1: List of reference(F)
% L, (F)
deleteTile(F, L1, L):-
	(head(F, A),
	last(F, B),
	B < A,
	delete(L1, [B,A], L),!);
	delete(L1, F ,L).

% deleteList(i, i, o): auxiliar for list
% [H|T]:List that we want to delte
% % L: list reference[H|T])
% NL:New List([H|T])
deleteList([H|T], L, NL):-
	deleteTile(H, L, L3),
	deleteList(T, L3, NL).
deleteList([], L, L):- !.

% head(i,o):Takes the list and returns the value of the head of the list
head([A|_], A).

printLine(0):- !.
printLine(N):-
	write("-------"),
	NN is N - 1,
	printLine(NN).



% if we stay with double tales if we lose we would have a higher score,
% so we assign -1 score if we have 2 or more double tales 
%1 score if we have 1 or none double tale
% %i,o
relationDT(Hand,Res):-
    doubleTales(Hand,Count),
    Count>=2,
    Res is -1;
    Res is 1.
%Counts number of double tales on the hand
%%i,o
doubleTales([X|Tail],[Y|Tail2]):-
    isDT(X,Res),
    Y is 0+Res,
    doubleTales(Tail,Tail2).

doubleTales([],[]):-!.

%Checks if the two number on the piece are the same
%%i,o
isDT([X|Tail],Res):-
    second(Tail,Num),
    X=:=Num,
    Res is 1;
    Res is 0.
second([X|_],Num):-
    Num is X.


%Relation between the number of tiles at our hand against the greater number of repeats number.
%%i,i,o
relation(Count,Bigger,Rel):-
    Count/Bigger>2,
    Rel is -1;
    Count/Bigger=<2,
    Rel is 1.
%Checks number of repeated number
%i,o
repeated(Hand,Rel):-
    Num is 0,
    count2(Hand,Num,Arr),
    numRepeated(Arr,Mayor),
    size(Hand,Count),
    relation(Count,Mayor,Rel).
%size of the list
%%i,o
size([_|Tail],Count):-
    size(Tail,C1),
    Count is C1+1.
size([],Count):-
    Count is 0,!.
%Gets the largest number of repetitions in the tiles.
%i,o
numRepeated([X|Tail],Bigger):-
    numRepeated2(Bigger,X,Tail).
numRepeated2(A,B,[X|Tail]):-
    X>=B,!,
    numRepeated2(A,X,Tail);
    X=<B,
    numRepeated2(A,B,Tail).
numRepeated2(Bigger,Bigger,[]):-!.



%Count the repetitions that there are for each number, the double tales count as one
% i,i,o
count2(Hand,Num, Freq):-
    Num=:=7, Freq = [];
    sum(Hand,Num, X),
    Num1 is Num+1,
    count2(Hand, Num1, Tail),
    Freq = [ X | Tail ].

%Count the repetitions for a specific number
% i,i,o
sum([X|Tail],Num,Count):-
    sum(Tail,Num,C1),
    verify(X,Num),
    Count is C1+1;
    sum(Tail,Num,C1),
    Count is C1.
sum([],_,Count):-
    Count is 0.
%verify that a number is equal to the first number on the tile
%i,o
verify([X|Tail],Num):-
    X=:=Num;
    verify2(Tail,Num).
%verify that a number is equal to the second number on the tile
verify2([X|_],Num):-
    X=:=Num.

sum_tile([X|Y], V):-
    V is X + Y.

sum_list1([], []):- !.

sum_list1([X|R], V):-
    sum_tile(X, SR),
    sum_list1(R, V1),
    V = [SR|V1].

