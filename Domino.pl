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

% es una lista de todas las fichas posibles 
initState([[-1],[[0, 0], [0, 1], [0, 2], [0, 3], [0, 4], [0, 5], [0, 6], [1, 1], [1, 2],[1, 3], [1, 4], [1, 5], [1, 6], [2, 2],[2, 3], [2, 4], [2, 5], [2, 6], [3, 3],[3, 4], [3, 5], [3, 6], [4, 4], [4, 5], [4, 6], [5, 5], [5, 6], [6, 6]],[]]).

% Print the Game State
% imprime el estado actual del juego
% Layout -> es la representacion del juego 
% Hand -> son las fichas que tenemos en nuestra mano actualmente
printGameDetails([Layout, Possible, Hand]):-
    format('Layout: ~w~nPossible Pieces: ~w~nMy Hand: ~w~n', [Layout, Possible, Hand]).

% Se usa para añdir fichas en el juego inicial y para añadir fichas cuado se requiere comer
% State -> es el estado inicial de la partda en ese momento
% Current -> es el contador de las fichas en ceros
% Current -> es el contador final de las fichas, se usa para saber cuantas fichas se van a insertar
% NewState -> es el estado final del juego en ese momento, puede cambiar el número de fichas y sus valores, es el parámetro de salida de la función
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


% comer fichas de domino cuando no tenemos
% State -> es el estado inicial del juego en ese momento de la partida
% NewState -> es el estado final de la partida después de cambios en el juego(cambios en el numero y valor de las fichas de domino)
funcion(State,NewState):-

    draw(State,0,1,NewState),!;
    nl.

% Determine what player is going to play first
% State-> es el estado actual de la partida
chooseTurn(State):-
    write("Am I going first? (y/n):"),
    nl,
    read(Y),
    nl,
    Y == y,
    myTurn(State);
    enemyTurn(State).

/*-------------------- Turns --------------------*/
% indica el turno del usuario
% State -> estado actual de la partida
myTurn(State):-
    printGameDetails(State), nl,
    write("-------------My Turn--------------"), nl,
    selectMyMove(State).
% indica el turno del contrincante
% State -> estado actual de la partida
enemyTurn(State):-
    printGameDetails(State) ,nl,
    write("--------------Enemy's Turn--------------"), nl,
    getEnemyInput(State).

/*-------------------- My moves --------------------*/



% Check if we can play and make an optimal move.
% selecciona el movimiento que se va a ejecutar(Comer o Jugar)
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
    printGameDetails(State),
    write("pon la fiha "),
	read(X_44),
    Move = X_44, %dummy move
    printGameDetails(State),
    performMyCommand(State, Move).
    

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
	write("Got to performEnemyCommand with tile- invalid input"),nl,
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
        nl,
	write("Played: " ),
	write( StatesH_Piece),
        nl,
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


% Create possible states per row:
% genStates( i, o), where i= Hand of the player.
genStates(Hand , PossibleHands):-
	genPossibleHands( [], Hand, PossibleHands).
genPossibleHands(_, [], []).
genPossibleHands(HandBuffer , [HandH| HandT], PossibleHands):-
	genPossibleHands( [ HandH | HandBuffer], HandT, PossibleHandsT ),
	mix(HandBuffer, HandT, AllMinusOne),
	PossibleHands = [ [HandH , AllMinusOne] | PossibleHandsT].

% Utility method, used by genStates.
mix([],List,List):-!.
mix([X|List1],List2,[X|List3]) :-
	mix(List1,List2,List3).


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


/*-------------------- Heuristica --------------------*/

%Sum of the score of three different heuristic methods
%i,i,o
heuristic(Hand,HandOp,Res):-
    repeated(Hand,ResRep),
    relationDT(Hand,ResDT),
    relationOp(HandOp,Hand,ResOp),
    Res is ResRep+ResOp+ResDT.


% if we stay with double tales if we lose we would have a higher score,
% so we assign -1 score if we have 2 or more double tales 
%1 score if we have 1 or none double tale
% %i,o
relationDT(Hand,Res):-
    doubleTales(Hand,Count),
    Count>=2,
    Res is -1;
    Res is 1.


    X=:=Num,
    Res is 1;
    Res is 0.
second([X|_],Num):-
    Num is X.

% If the opponent has more pieces we have a positive score
%i,i,o
relationOp(HandOp,Hand,Res):-
    size(HandOp,CountO),
    size(Hand,CountP),
    Res is CountO-CountP.

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



%Count the repetitions that there are for each nuber, the double tales count as one
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


zs_max_at(Zs,Max,Pos):-
    maplist(#>=(Max),Zs),
    nth0(Pos,Zs,Max).
