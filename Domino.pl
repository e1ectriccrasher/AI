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
	nb_setval(handOp, [[0,0],[0,1],[0,2],[0,3],[0,4],[0,5],[0,6],[1,1],[1,2],[1,3],[1,4],[1,5],[1,6],[2,2],[2,3],[2,4],[2,5],[2,6],[3,3],[3,4],[3,5],[3,6],[4,4],[4,5],[4,6],[5,5],[5,6],[6,6]]),
	nb_setval(numHand, 7),
	nb_setval(numHandOp, 7),
	nb_setval(layout, []),
	write("Enter your tiles (Format [[A,B],[C,D]...]: "), nl,
	read(A), nl,
	nb_setval(hand, A),
	nb_getval(handOp, Op),
	deleteList(A, Op, NOp),
	nb_setval(handOp, NOp),
	write("You start the game? (y/n) "), nl,
	read(Prim),
	firstStep(Prim, T),
	play(T).

% firstStep(i, o): Makes first move and continue game 

% case n: Opponent throw first tile
%  Put on the layout
%  Changes list from opponent(handOp), possible pieces on the hand of opponent
% decrease (numHandOp)
firstStep(n, T):-
	write("Enter tile from opponent: "), nl,
	read(F),
	nb_setval(layout, [F,F]),
	nb_setval(numHandOp, 6),
	nb_getval(handOp, HandOp),
	deleteTile(F, HandOp, NewHand),
	nb_setval(handOp, NewHand),
	T is 1.

% case y: we put the first tile
% Put on the layout
% changes tile on hand(hand)
% decrease(numHand)
firstStep(y, T):-
	write("Enter first tile: "), nl,
	read(F),
	nb_setval(layout, [F,F]),
	nb_setval(numHand, 6),
	nb_getval(hand, Hand),
	deleteTile(F, Hand, NewHand),
	nb_setval(hand, NewHand),
	T is 0.

% play(i): method to make all the moves ours/opponent
% case 0: opponent turn
% checks if he won
% Print layout
% Ask if the opponent took tile
% Ask if opponent throws on head or tail
% calls opponentTurn(i, i)
% calls play(1) to change turn
play(0):-
	nb_getval(numHand, Num),
	((Num < 1, write("----------YOU WON!!!!!!!!------------"), nl, !);
	(nb_getval(layout, Tab),
	length(Tab, Len),
	write("LAYOUT: "), nl,
	write("----"), printLine(Len), write("----"), nl,
	write("|"), write(Tab), write("|"), nl,
	write("----"), printLine(Len), write("----"), nl,
	nl, write("How many does the opponent take?"), nl,
	read(N),
	write("Enter number: "),nl,nl,
	write("-1: throws on the head"),nl,
	write(" 0: if it did not throw"),nl,
	write(" 1: throws on tail"), nl,nl,
	read(U),
	opponentTurn(N, U), nl,
	play(1))).

%Case 1:
%Check if the opponent already won
%Print layout
%print tiles from player
%calls makemove()
%calls play(0) to change turn
%
play(1):-
	nb_getval(numHandOp, Num),
	write("The opponent has "), write(Num), write(" tiles"), nl,
	((Num < 1, write("---------YOU LOST!!!!!!!!-------------"), nl, !);
	(nb_getval(layout, Tab),
	length(Tab, Len),
	write("LAYOUT: "), nl,
	write("--"), printLine(Len), write("--"), nl,
	write("|"), write(Tab), write("|"), nl,
	write("--"), printLine(Len), write("--"), nl,
	nb_getval(hand, Hand),
	nl, write("YOUR HAND: "), write(Hand), nl,
	makeMove(), nl,
	play(0))).

%update value of numHandOp and update layout if the opponent throw a tile
%We have three cases
%Case 0: the opponent did not throw
%Case 1: the opponent throws tile on tail
%Case -1: the opponent throws tile on head

opponentTurn(N, 0):-
	checkTie(),
	nb_getval(numHandOp, M),
	NewNum is N + M,
	nb_setval(numHandOp, NewNum),
	nb_setval(tiee, 1).


opponentTurn(N, V):-
	write("Put opponent tile: "), nl,
	read(F),
	changeTile(F, V, NF),
	settle(V, NF),
	nb_getval(handOp, Op),
	deleteTile(NF, Op, NOp),
	nb_setval(handOp, NOp),
	nb_getval(numHandOp, M),
	NewNum is (N + M - 1),
	nb_setval(tiee, 0),
	nb_setval(numHandOp, NewNum).

changeTile(F, 1, NF):-
	head(F, HF), last(F, TF),
	nb_getval(layout, Tab),
	last(Tab, Ult), last(Ult, TT),
	((HF =:= TT, NF = F);
		(NF = [TF, HF])).

changeTile(F, -1, NF):-
	head(F, HF), last(F, TF),
	nb_getval(layout, Tab),
	head(Tab, Prim), head(Prim, HT),
	((TF =:= HT, NF = F);
		(NF =[TF, HF])).

% settle(i, i): adjust the tile to match the layout 

% Case -1: adjust the tile (F) on the head of layout
settle(-1, F):- 
	nb_getval(layout, Tab),
	nb_setval(layout, [F|Tab]).

% Case 1: adjust the tile (F) on the tail of layout
settle(1, F):- 
	nb_getval(layout, Tab),
	union(Tab,[F], NT),
	nb_setval(layout, NT).

% Case 2: Search the option of position
settle(2, F):-
	nb_getval(layout, [[HT|_]|B]),
	head(F, HF), last(F, TF), 
	last(B, UF), last(UF, TT),	(
		(HF =:= HT, settle(-1,[TF,HF]));
		(TF =:= HT, settle(-1, F));
		(HF =:= TT, settle(1, F));
		(TF =:= TT, settle(1, [TF,HF])),!).


%MakeMove: main predicate to make the riht move on the board
%calls find to look for the possible moves
%calls takeNewTile if we do not have any valid move
%calls adjust if the list has only one option
%calls alphabeta if the list>1
%
makeMove:-
	nb_getval(hand, Hand),
	nb_getval(layout, [[A1|_]|B]),
	nb_getval(numHand, NumFichas),
	NewNum is NumFichas - 1,
	find(Hand, A1, [], D),
	last(B, L),
	last(L, L1),
	find(Hand, L1, D, PF), 
	((PF == [], 
		takeNewTile(Hand, NumFichas));
	(length(PF, 1),
		write("Possible moves: "), write(PF), nl,
	 	head(PF, F), 
		deleteTile(F, Hand, NewHand), 
	 	nb_setval(hand, NewHand), 
	 	settle(2, F), 
	 	nb_setval(tiee, 0),
	 	nb_setval(numHand, NewNum),
	 	nl, write("Best Move: "), write(F), nl);
	(write("Possibles moves: "), write(PF), nl,
		nb_getval(layout, Layout),

		nb_getval(handOp, HandOp),
		alphabeta(Layout, 3, HandOp, Hand, Hand, 1, -10000,  10000, BestMove, Value), 
		write("Value de la rama: "), write(Value), nl,
		nl, write("Best move: "), write(BestMove), nl,
		deleteTile(BestMove, Hand, NewHand), nb_setval(hand, NewHand),
		nb_setval(tiee, 0),
		nb_setval(numHand, NewNum),
		settle(2, BestMove),!
		)).

%TakeNewTile: method that put the new tile on the hand
%A: list of tiles
%L: number of tiles
%Ask if they are tile to take
%Put the new tile on the hand
%Delete the tile from handop
%calls makeMove
takeNewTile(A, L):-
	% write("Escribe 1 si hay hand para takeNewTile o 0 si ya se acabaron las hand"), nl,
	% read(Value),
	nb_getval(numHandOp, NumHandOp),
	nb_getval(handOp, HandOp),
	length(HandOp, Len),
	Value is Len - NumHandOp,
	((Value =:= 0, write("PASS"), nl,
	nb_getval(tiee, Tiee),
	checkTie(Tiee));
	(write("Put new tile: "), nl,
	read(F),
	nb_setval(hand, [F|A]),
	nb_getval(handOp, Op),
	deleteTile(F, Op, NOp),
	nb_setval(handOp, NOp),
	LN is (L + 1),
	nb_setval(numHand, LN),
	nl, write("You have "), write(LN), write(" tiles"), nl,
	makeMove())).

%checks if the oppponent did not throw and the game is blocked
checkTie(0):-
	nb_setval(tiee, 1),
	play(0).

checkTie(1):-
	write("TIE!!!!"), nl, fail.

% ----------------------------------------------------- MINIMAX Y FUNCIÓN HEURÍSTICA ------------------------------------------------------------------------------------

%putTitle
%update the layout 
%delete the move from the hand
%returns a newLayout

putTile(Hand, Hit, Layout, NewHand, NewLayout):-
	completeLayout(Hit, Layout, NewLayout),
	deleteTile(Hit, Hand, NewHand).

%Complete layout
%take the head
%take the tail
%compare with the head and tail of the tile to play
%
completeLayout(Hit, Layout, NewLayout):-
	nb_setval(tab, Layout),
	nb_getval(tab, [[HT|_]|B]),
	head(Hit, HF), last(Hit, TF),
	last(B, UF), last(UF, TT), 
	((HF =:= HT, adjust(-1,[TF,HF], Layout, NewLayout));
	(TF =:= HT, adjust(-1, Hit, Layout, NewLayout));
	(HF =:= TT, adjust(1, Hit, Layout, NewLayout));
	(TF =:= TT, adjust(1, [TF,HF], Layout, NewLayout)), !).

%adjust the tile to make sense on the layout
adjust(-1, Hit, Layout, NewLayout):- 
	union([Hit], Layout, NewLayout).


adjust(1, Hit, Layout, NewLayout):- 
	union(Layout, [Hit], NewLayout). 

/*------------------------------------------------ALPHABETA------------------------*/
 %ALPHABETA 
 %the method that implements the algorithm of minmax with alphabeta to make the bestmove
 %Case1: Hand is empty.
alphabeta(_, _, HideTiles, [], _, _, _, _, _, Value):-
	length(HideTiles, N1),
	Value is N1, !.

%Case2: We cannot put a tile.
%We have to pass or takeNewTile
%
alphabeta(MaxMin, _, HideTiles, MyTiles, [], _, _, _, _, Value):-
	length(HideTiles, N1), length(MyTiles, N2),
	Value is (N1 - N2) * (-MaxMin), !.

%Case3:we reach the max depth 
%
alphabeta(_, 0, HideTiles, MyTiles, _, _, _, _, _, Value):-
	length(HideTiles, N1), length(MyTiles, N2), 
	Value is (N1 - N2), !.

%Case4:HandOp is empty 
%
alphabeta(_, _, [], MyTiles, _, _, _, _, _, Value):-
	length(MyTiles, N2),
	Value is -N2, !.

%layout: is the board on the game
%Depth: depth of search (arbitrary)
%player: the player on the game. 1 for the user
%alfa beta: given from user
%Hand: all the tiles of the user
%HandOp: hidden tiles from opponent
%Search: tiles from player that we use for making the best choice
%RETURN THE BESTMOVE
alphabeta(Layout, Depth, HideTiles, MyTiles, Search, MaxMin, Alfa, Beta, Movement, Value):-
	Depth > 0,
	nb_setval(tabProv, Layout),
	nb_getval(tabProv, [[A1|_]|B]),
	find(Search, A1, [], D), 
	last(B, L),
	last(L, L1),
	find(Search, L1, D, Possibility),
	Alfa1 is -Beta,
	Beta1 is -Alfa,	
	NuevaP is Depth - 1,
	move(MaxMin, Possibility, Layout, HideTiles, MyTiles, Search, NuevaP, Alfa1, Beta1, _, (Movement, Value)), !.



move(_, [], _, _, _, _, _, Alfa, _, Hit, (Hit, Alfa)).

move(MaxMin, [Hit | Left], Layout, HideTiles, MyTiles, Search, Depth, Alfa, Beta, Record, BestMove):-
	putTile(Search, Hit, Layout, NewHand, NewLayout),
	changePlayer(MaxMin, MaxMin2),
	putNewTile(Hit, Left, MaxMin2, NewLayout, Depth, HideTiles, MyTiles, NewHand, Alfa, Beta, Record, BestMove, _), !.


putNewTile(Hit, Left, 1, Layout, Depth, _, MyTiles, NewHand, Alfa, Beta, Record, BestMove, Value):-
	alphabeta(Layout, Depth, NewHand, MyTiles, MyTiles, 1, Alfa, Beta, BestMove, Value),
	Valor1 is -Value,
	poda(1, Hit, Valor1, Depth, Alfa, Beta, Left, Layout, NewHand, MyTiles, MyTiles, Record, BestMove).


putNewTile(Hit, Left, -1, Layout, Depth, HideTiles, _, NewHand, Alfa, Beta, Record, BestMove, Value):-
	alphabeta(Layout, Depth, HideTiles, NewHand, HideTiles, -1, Alfa, Beta, BestMove, Value),
	Valor1 is -Value,
	poda(-1, Hit, Valor1, Depth, Alfa, Beta, Left, Layout, HideTiles, NewHand, HideTiles, Record, BestMove).


poda(_, Hit, Value, _, _, Beta , _ , _, _, _, _, _, (Hit, Value)) :- 
   Value >= Beta, !.


poda(MaxMin, Hit, Value, Depth, Alfa, Beta, Left, Layout, HideTiles, MyTiles, Search, _, BestMove) :- 
   Alfa < Value, Value < Beta, !, 
   move(MaxMin, Left, Layout, HideTiles, MyTiles, Search, Depth, Value, Beta, Hit, BestMove).


poda(MaxMin, _, Value, Depth, Alfa, Beta, Left, Layout, HideTiles, MyTiles, Search, Record, BestMove) :- 
   Value =< Alfa, !, 
   move(MaxMin, Left, Layout, HideTiles, MyTiles, Search, Depth, Alfa, Beta, Record, BestMove).


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
