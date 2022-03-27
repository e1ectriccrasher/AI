%%% Prolog progam that plays Domino by entering the
%%% tiles the players have.

%%% 0
domino([0,0]).

%%% 1
domino([0,1]).
domino([1,1]).

%%% 2
domino([0,2]).
domino([1,2]).
domino([2,2]).

%%% 3
domino([0,3]).
domino([1,3]).
domino([2,3]).
domino([3,3]).

domino([0,4]).
domino([1,4]).
domino([2,4]).
domino([3,4]).
domino([4,4]).

domino([0,5]).
domino([1,5]).
domino([2,5]).
domino([3,5]).
domino([4,5]).
domino([5,5]).

domino([0,6]).
domino([1,6]).
domino([2,6]).
domino([3,6]).
domino([4,6]).
domino([5,6]).
domino([6,6]).




read_tiles:-
    L=[],
    read(X),
    is_list(X),
    is_domino_l(X),
    append(X,L,F),
    write('Current '),write(F),nl,
    assertz(F).

is_domino_l([]):-!.
is_domino_l([D|R]):-
    is_list(D),
    length(D,2),
    is_domino(D),
    is_domino_l(R),
    !.

is_domino([X|Y]):-
    X < 7,
    Y < 7,
    X >= 0,
    Y >= 0.



/* The alpha-beta algorithm from textbook
*/
alphabeta( Pos, Alpha, Beta, GoodPos-GoodPosMeta, Val):-
	moves( Pos, PosList), !,
	boundedbest( PosList, Alpha, Beta, GoodPos-GoodPosMeta, Val)
	;
	h( Pos, Val).                              % Static value of Pos

boundedbest( [Pos-Meta | PosList], Alpha, Beta, GoodPos-GoodPosMeta, GoodVal):-
	alphabeta( Pos, Alpha, Beta, _, Val),
	goodenough( PosList, Alpha, Beta, Pos-Meta, Val, GoodPos-GoodPosMeta, GoodVal).

goodenough( [], _, _, Pos, Val, Pos, Val)  :-  !.    % No other candidate

goodenough( _, Alpha, Beta, Pos, Val, Pos, Val)  :-
	min_to_move( Pos), Val > Beta, !                   % Maximizer attained upper bound
	;
	max_to_move( Pos), Val < Alpha, !.                 % Minimizer attained lower bound

goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal)  :-
	newbounds( Alpha, Beta, Pos, Val, NewAlpha, NewBeta),    % Refine bounds
	boundedbest( PosList, NewAlpha, NewBeta, Pos1, Val1),
	betterof( Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds( Alpha, Beta, Pos, Val, Val, Beta)  :-
	min_to_move( Pos), Val > Alpha, !.                 % Maximizer increased lower bound

newbounds( Alpha, Beta, Pos, Val, Alpha, Val)  :-
	max_to_move( Pos), Val < Beta, !.                 % Minimizer decreased upper bound

newbounds( Alpha, Beta, _, _, Alpha, Beta).          % Otherwise bounds unchanged

betterof( Pos, Val, _, Val1, Pos, Val)  :-        % Pos better than Pos1
	min_to_move( Pos), Val > Val1, !
	;
	max_to_move( Pos), Val < Val1, !.

betterof( _, _, Pos1, Val1, Pos1, Val1).             % Otherwise Pos1 better
