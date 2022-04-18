/*-------------------- Instructions --------------------*/
% You can start the game using:
% startGame.


% Still implementing:

% Need to finish! This method returns the possible pieces in the enemy hand
% This method is suposed to get a list containing the possible domino tiles that can be played by us.
% write("What is the enemy doing (d: draw| p: playing):"),

/*-------------------- Board & Game Engine --------------------*/

% Main predicate:
startGame():-
    % Create the board State: [Layout, Possible Pieces, userHand]
    initState(State),
    % Add 7 new pieces to the hand
    draw(State,0,1, NewState),
    % Check whos first and start game.
    chooseTurn(NewState).

% Contains Empty Layout, All Domino Tiles(28) & Empty hand.
initState([[-1],[[0, 0], [0, 1], [0, 2], [0, 3], [0, 4], [0, 5], [0, 6], [1, 1], [1, 2],[1, 3], [1, 4], [1, 5], [1, 6], [2, 2],[2, 3], [2, 4], [2, 5], [2, 6], [3, 3],[3, 4], [3, 5], [3, 6], [4, 4], [4, 5], [4, 6], [5, 5], [5, 6], [6, 6]],[]]).

% Print the Game State
printGameDetails([Layout, Possible, Hand]):-
    format('Layout: ~w~nPossible Pieces: ~w~nMy Hand: ~w~n', [Layout, Possible, Hand]).

% draw(0, N) := Take N pieces querying the user.
draw(State, Current, Current, State).
draw(State, Current, EndCondition, NewState):-
    [Layout, Possible, Hand] = State,
    !,
    Current=\=1,
    write("Insert piece value [X, Y]: "), %Pieces must be inserted in the order found in Possible Pieces, and with [].
    nl,
    read(X),
    [Side1, Side2] = X,
    append([X], Hand, NewHand),			% Add the drawn piece to hand
    delete(Possible, [Side1, Side2], NewPossible), % Remove drawn piece from possible pieces
    delete(NewPossible, [Side2, Side1], NewPossible2),
    Next is Current + 1,
    draw([Layout, NewPossible2, NewHand] , Next, EndCondition, NewState).



funcion(State,NewState):-

    draw(State,0,1,NewState),!;
    nl.

% Determine what player is going to play first
chooseTurn(State):-
    write("Am I going first? (y/n):"),
    nl,
    read(Y),
    nl,
    Y == y,
    myTurn(State);
    enemyTurn(State).

/*-------------------- Turns --------------------*/
myTurn(State):-
    printGameDetails(State), nl,
    write("-------------My Turn--------------"), nl,
    selectMyMove(State).

enemyTurn(State):-
    printGameDetails(State) ,nl,
    write("--------------Enemy's Turn--------------"), nl,
    getEnemyInput(State).

/*-------------------- My moves --------------------*/



% Check if we can play and make an optimal move.
selectMyMove(State):-
    %[_, _, Hand] = State,
    % if hand is empty, got to performMyCommand(bla, draw, bla),

    % Check if we can play something, if not, performMyCommand(bla, pass, bla),

    % Get a smart sublist of the hand, checking what can be played,

    % Run alfabeta with that info, getting the best tile we can play

    %genStates(Hand, InterestingMoves),
    % alphabeta(4, -3000, 3000, InterestingMoves, Possible , BestMove, 1);
    % Last but not least, perform that command
    
    write(' vas a comer?(y/n) '),
    read(X_1),
    X_1=y,
    funcion(State,NewState),selectMyMove(NewState);
    
    % ======================================== esto es para poner la ficha
    write("pon la fiha "),
	read(X_44),
    Move = X_44, %dummy move
    performMyCommand(State, Move).
    %printGameDetails(State).

% performMyCommand(State, Side, Move)
performMyCommand([Layout, Possible, Hand], [Side1, Side2]):-
    % Remove the piece moved from my hand
    delete(Hand, [Side1, Side2], NewHand),
    delete(NewHand, [Side2, Side1], NewHand2),
    % Place the piece in the Layout:
    insertDomino( [Side1, Side2], Layout, NewLayout),
    format("I placed ~w.~n", [[Side1,Side2]] ),
    % Start enemyTurn with the new State
    enemyTurn([NewLayout, Possible, NewHand2]). %Falla

/*-------------------- Enemy Moves--------------------*/
% Manage the enemy turn
getEnemyInput(State) :-
    % !: Need to add if the oponent had to take from pit
    write("Enemy placed tiles? (y/n): "),
    read(X), nl,
    X == y,
    ((write("Enter Domino tile [X,Y]:"),
      read(DominoTile),
      performEnemyCommand(State, DominoTile));
    performEnemyCommand(State,0)).

/* Enemy draws or passes */
performEnemyCommand(State, 0) :-
   [_, PossiblePieces, _] = State,
   size(PossiblePieces, Count),  % Check size of Possible Pieces, if Possible Pieces is >=1
   Count >= 1, % Update number of enemy pieces. (+1)
   getEnemyInput(State);
   myTurn(State).

% Insert Domino on the board
performEnemyCommand( [Layout, Possible, Hand], [Side1, Side2]) :-
    write("Got to performEnemyCommand with tile"),nl,
    delete(Possible, [Side1, Side2], NewPossible), %It deletes the piece played from possible pieces because it is now revealed
    delete(NewPossible, [Side2, Side1], NewPossible2),
    insertDomino([Side1, Side2], Layout, NewLayout), %As it is an insertion on the left it pushes the piece into the layout, creating a new layout.
    format("Enemy placed ~w. ~n", [[Side1,Side2]]),
    % Pass new state to the enxt turn.
    myTurn([NewLayout, NewPossible2, Hand]).

% -------------------- Convenience methods
% Insert element to the front of the list
pushToFront(Element, List, NewList):-
	append([Element], List, NewList).
% Insert element to the end of the list
pushToEnd(Element, List, NewList) :-
	append(List, [Element], NewList).

% Get the first element of the list
getFirstElement( [Element | _] , Element):-!.
% Get the last element of the list
getLastElement( [Element | []], Element):-!.
getLastElement([_ | Tails], Element):-!,
	getLastElement(Tails, Element).

% Insert the first tile in the game:
insertDomino([Side1, Side2], [-1], NewLayout):-
	write("Inserting the first piece"), nl,
	NewLayout = [[Side1, Side2]].

% Insert an arbitrary domino piece
insertDomino([Side1, Side2], Layout, NewLayout ):-
	getFirstElement(Layout, [First1, _]),
	getLastElement(Layout, [_, Last2]),
	(	% Check at the beginning of List
		Side1 == First1,
		pushToFront([Side2, Side1], Layout, NewLayout);
		Side2 == First1,
		pushToFront([Side1, Side2], Layout, NewLayout);
		% Check at the end of the List
		Side1 == Last2,
		pushToEnd([Side1, Side2], Layout, NewLayout);
		Side2 == Last2,
		pushToFront([Side2, Side1], Layout, NewLayout)
	).

% Remove elements contained in a List from another List.
% Special cases
deleteListFromList( [], _, []):-!.
deleteListFromList(List, [], List):-!.

deleteListFromList(List1, [ L2H| []], Result):-
delete(List1, L2H, Result), !.
deleteListFromList(List1, [L2H |L2T], Result):-
	deleteListFromList(List1, L2T, SubResult),
	delete(SubResult, L2H, Result).

/*-------------------- Alphabeta --------------------*/
test:-
	genStates([1,2,3], PossibleHands),
	write("PossibleHands are: "),
	write(PossibleHands),
	nl,
	alphabeta(4, -3000, 3000, PossibleHands, [4,5] , BestMove, 0),
	nl,nl,nl,
	write("Result is: "),
	write( BestMove),
	write(".").

% Depth is 0
alphabeta(-1, _, _, -, _ , BestMove,_):-
	% This should call the heuristic function
	write("Calling heuristic clause: "),nl,nl,
	heuristic(BestMove).


%There is no more states to explore.
% X is the last piece the player can place.
alphabeta( _, _, _, [ [_,[]] ], [], BestMove,_):-
	write("Terminated"),
	nl,nl,
	heuristic(BestMove).

%There is no more states to explore at the same level.
% Need to remove -3000
alphabeta( _, _, _, [], _, BestMove,_):-
	BestMove is -543231,
	write("Reached end of line").

% Main alphabeta method (for the user)
alphabeta( Depth, Alpha, Beta,[ [StatesH_Piece, StatesH_Hand] | StatesT], Storage, BestMove,0):-
	write("Depth "),
	write(Depth),
	write("| "),
	write("Played: " ),
	write( StatesH_Piece),
	write(", Starting alphabeta. "),
	write("Going to pass: "),
	write(StatesH_Hand),
	write(" left: "),
	write(StatesT),
	write(". Storage: "),
	write(Storage),
	nl,

	% Compute one level down branches:
	genStates(Storage, PossibleHands),

	write("Finished creating possible moves, possible hands: "),
	write(PossibleHands),
	nl,
	NextLevel is Depth-1,
	alphabeta(NextLevel, Alpha, Beta, PossibleHands, StatesH_Hand, BestMove1, 1),
	%Test is BestMove1 + 1,
	%max(Test,Alpha, BestMove),
	write("___in depth:"),
	write(Depth),
	write(" played: "),
	write(StatesH_Piece),
	write("Value of BestMove1: "),
	write(BestMove1),
	max(Alpha,  BestMove1, MaxedAlpha),

	%Beta > Alpha,

	% Compute same level branches:
	alphabeta( Depth, MaxedAlpha, Beta, StatesT, Storage, BestMove2, 1),
	max(MaxedAlpha, BestMove2, BestMove).

% Main alphabeta method (for the enemy)
alphabeta( Depth, Alpha, Beta,[ [StatesH_Piece, StatesH_Hand] | StatesT], Storage, BestMove, 1):-

	write("Depth "),
	write(Depth),
	write("| "),
	write("Played: " ),
	write( StatesH_Piece),
	write(", Starting alphabeta. "),
	write("Going to pass: "),
	write(StatesH_Hand),
	write(" left: "),
	write(StatesT),
	write(". Storage: "),
	write(Storage),
	nl,

	% Compute one level down branches:
	genStates(Storage, PossibleHands),

	write("Finished creating possible moves, possible hands: "),
	write(PossibleHands),
	nl,
	NextLevel is Depth-1,
	alphabeta(NextLevel, Alpha, Beta, PossibleHands, StatesH_Hand, BestMove1, 0),
	%Test is BestMove1 + 1,
	%max(Test,Alpha, BestMove),
	write("___in depth:"),
	write(Depth),
	write(" played: "),
	write(StatesH_Piece),
	write("Value of BestMove1: "),
	write(BestMove1),
	min(Beta,  BestMove1, MinimizedBeta),

	% Compute same level branches:
	alphabeta( Depth, Alpha, MinimizedBeta, StatesT, Storage, BestMove2, 0),
	min(MinimizedBeta, BestMove2, BestMove).

% Mock method for the heuristic.
heuristic(Position):-
Position = 1.


% Create possible states per row:
% genStates( i, o), where i= Hand of the player.
genStates( Hand , PossibleHands):-
	genPossibleHands( [], Hand, PossibleHands).
genPossibleHands(_, [], []).
genPossibleHands(HandBuffer , [HandH| HandT], PossibleHands):-
	genPossibleHands( [ HandH | HandBuffer], HandT, PossibleHandsT ),
	combina(HandBuffer, HandT, AllMinusOne),
	PossibleHands = [ [HandH , AllMinusOne] | PossibleHandsT].

% Utility method, used by genStates.
combina([],Lista,Lista):-!.
combina([X|Lista1],Lista2,[X|Lista3]) :-
	combina(Lista1,Lista2,Lista3).


% Used in alphabeta:
max( X, Y, Z):-
write("Going to maximize, X:"),
write(X),
write(" , Y:"),
write(Y),
write(" "),
	X >=Y,
	Z is X,
	!.
max(_, Y, Y).

min( X, Y, Z):-
	X =< Y,
	Z is X,
	!.
min(_, Y, Y).

/%
%Counts the number of elements in a list
%count([], 0).
%count( [ Head | Tails ], N):-
%	count(Tails, N1),
%	N is N1 + 1.
%/

% ============================================= Print Matrix(i)
/*
This clause traverses the matrix as in the "sum" clause, while printing
the matrix entries.
*/

printList([ [] | [] ]):- nl,nl,!.

%End of Row?
printList([ []|ColumsA]):-
	nl, printList(ColumsA).

%Main Method
printList([[EntryA | ColumnsA] | RowsA]):-
	write(EntryA),write(" "),
	printList([ColumnsA|RowsA]).

% Emulate a loop.
repeat.

repeat:-
	repeat.


% Confirm that the Data passed by the user is valid,
% Give the user a second chance to insert data.
confirm(Data):-
write("Is "),
write(Data),
write(" correct? (y/n)"),
nl,
read(YesNo),
YesNo == y.


/*-------------------- Heuristica --------------------*/
listaSimpleAListaListas([X,Y|Cola],[A|Cola2]):-
    creaLista(X,Y,A),
    listaSimpleAListaListas(Cola,Cola2).

listaSimpleAListaListas([],_):-!.

creaLista(X,Y,[A,B]):-
    A is X,
    B is Y.


%En esta función se sumarán todos los puntajes de distintos
%métodos heurísticos. Por ahora van 3
%i,i,o
heuristica(Mano,ManoOponente,Res):-
    repetidas(Mano,ResRep),
    relacionOponente(ManoOponente,Mano,ResOp),
    relacionMulas(Mano,ResMul),
    Res is ResRep+ResOp+ResMul.


% Si nos vamos quedando con mulas puede perjudicarnos, así que le
% asignamos un puntaje negativo al tener dos mulas o más y uno positivo
% al tener 1 o ninguna.
%i,o
relacionMulas(Mano,Res):-
    mulas(Mano,Count),
    Count>=2,
    Res is -2;
    Res is 1.
%Cuenta el número de mulas que se tienen llamando al predicado esMula
%i,o
mulas([X|Cola], Count):-
    esMula(X,Res),
    mulas(Cola,C1),
    Count is C1+Res.
mulas([],0):-!.
%Revisa si el primero y segundo numero de la ficha es el mismo,
%entonces es mula
%i,o
esMula([X|Cola],Res):-
    segundoNum(Cola,Num),
    X=:=Num,
    Res is 1;
    Res is 0.
segundoNum([X|_],Num):-
    Num is X.

% Si el oponente tiene más fichas entonces Res será positivo para
% nosotros
%i,i,o
relacionOponente(ManoOponente,ManoPropia,Res):-
    size(ManoOponente,CountO),
    size(ManoPropia,CountP),
    Res is CountO-CountP.

%Se ve la relación entre el numero de fichas en la mano del usuario contra la
% cantidad mayor de repetidas, por ejemplo, si tenemos 4 fichas con el numero 5
% , 2 fichas con el número 2, etc. usariamos 4. Si la relación es de más del 50%
% para las fichas repetidas con respecto del número de fichas en la mano entonces
% asignamos un valor positivo al estado.
%i,i,o
relacion(Count,Mayor,Rel):-
    Count/Mayor>2,
    Rel is -3;
    Count/Mayor=<2,
    Rel is 3.
%Se revisa el numero de fichas con el mismo número en la mano del usuario,
%Si tenemos mula solo contamos como 1 repeticion
%i,o
repetidas(Mano,Rel):-
    Num is 0,
    cuenta(Mano,Num,Arr),
    numRepetidas(Arr,Mayor),
    size(Mano,Count),
    relacion(Count,Mayor,Rel).
%Obtiene el tamaño de la lista
%i,o
size([_|Cola],Count):-
    size(Cola,C1),
    Count is C1+1.
size([],Count):-
    Count is 0,!.
%Obtiene el número mayor de repeticiones en las fichas, por ejemplo, si tenemos
% 4 fichas con el numero 5, 2 fichas con el número 2, etc. regresaría 4. No nos
% interesa tanto qué ficha es la que se repite más, solamente que el número de
% repeticiones sea alto
%i,o
numRepetidas([X|Cola],Mayor):-
    numRepetidas2(Mayor,X,Cola).
numRepetidas2(A,B,[X|Cola]):-
    X>=B,!,
    numRepetidas2(A,X,Cola);
    X=<B,
    numRepetidas2(A,B,Cola).
numRepetidas2(Mayor,Mayor,[]):-!.



%cuenta las repeticiones que hay por cada numero, las mulas cuentan 1 repeticion
% i,i,o
cuenta(Mano,Num, Freq):-
    Num=:=7, Freq = [];
    suma(Mano,Num, X),
    Num1 is Num+1,
    cuenta(Mano, Num1, Cola),
    Freq = [ X | Cola ].

%cuenta las repeticiones que hay para un numero en específico, las mulas cuentan 1 repeticion
% i,i,o
suma([X|Cola],Num,Count):-
    suma(Cola,Num,C1),
    verifica(X,Num),
    Count is C1+1;
    suma(Cola,Num,C1),
    Count is C1.
suma([],_,Count):-
    Count is 0.
%verifica que un número en específico sea igual a la primera entrada de una ficha
%i,o
verifica([X|Cola],Num):-
    X=:=Num;
    verifica2(Cola,Num).
%verifica que un número en específico sea igual a la segunda entrada de una ficha
%i,o
verifica2([X|_],Num):-
    X=:=Num.
