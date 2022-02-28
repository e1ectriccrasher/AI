%%% Calculates the degree of the polynomial

degree([K|Poly], N) :- 
  	K \= 0, length(Poly, N).
degree([_|Poly], N) :- 
  	degree(Poly, N).
	
% ?- degree([4,2],X).
%%% Adds two polynomials.

plus([],[],[]).

plus([KA|PA], [KB|PB], [KS|PS]) :- 
  	length(PA, NA), length(PB, NB), NA > NB,
	plus(PA, [KB|PB], PS), KS is KA.

plus([KA|PA], [KB|PB], [KS|PS]) :- 
  	length(PA, NA), length(PB, NB), NA < NB,
	plus([KA|PA], PB, PS), KS is KB.

plus([KA|PA], [KB|PB], [KS|PS]) :- 
  	plus(PA, PB, PS), KS is KA + KB.
% ?- plus([1,2],[2,3],X).
% ?- plus([4,8],[5,5],[9,13]).

%%% Substracts two polynomials

minus([],[],[]).

minus([KA|PA], [KB|PB], [KS|PS]) :- 
	length(PA, NA), length(PB, NB), NA > NB,
	minus(PA, [KB|PB], PS), KS is KA.
	
minus([KA|PA], [KB|PB], [KS|PS]) :- 
	length(PA, NA), length(PB, NB), NA < NB,
	minus([KA|PA], PB, PS), KS is -KB.
	
minus([KA|PA], [KB|PB], [KS|PS]) :- 
	minus(PA, PB, PS), KS is KA - KB.

% ?- minus([5,2],[3],[5,-1]).
% ?- minus([5],[3],[2]).


%%% Applies Horner's method to a polynomial

horner([], _, 0).

horner([K|P], X, R):-
	horner(P,X,RA),
	R is RA * X + K.
	
% ?- horner([5,2],3,X).
% ?- horner([61,2,24],5,671).
	
reverse([], P, P).
reverse([K|P], RP, A) :-
    reverse(P, RP, [K|A]).

%%% Evaluate polynomial P with X and output R

evaluate(P, X, R) :-
    reverse(P, PR),
    horner(PR, X, R).

%%% Differentiate polynomials

differentiate([K|P], [KR|PR]) :-
    length([K|P], D),
    diff([K|P], D, [KR|PR]).

diff(_, D, []) :-
    D =:= 1.

diff([K|P], D, [KR|PR]) :-
    diff(P, D - 1, PR),
    KR is (D - 1) * K.

% ?- differentiate([3,2,5],X).
% ?- differentiate([3,2,5],

%RESTA

%caso primer parametro vacio|
resta_pol([],X,X):- 
    !.
%caso segundo parametro vacio
resta_pol(X,[],X):- 
    !.
% caso de todos los parametros llenos
resta_pol([Uno|A], [Dos|B], [Resp|C]) :-
   Resp is Uno-Dos,
   resta_pol(A, B, C).

%?-resta_pol([5,6],[2,3],X).
