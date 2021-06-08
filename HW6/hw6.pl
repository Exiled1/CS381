/*
	GROUP 38FUN
	Mitchell Radford
	Rudy Peralta
	Tu Lam
	John Pierre Carr
	Hugh MacWilliams
*/


/* Exercise 1 */
redefine_system_predicate(when).

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).


% PART A
schedule(N, P, T) :-
    enroll(N, X), where(X, P), when(X, T).

% PART B
usage(P, T) :- where(X, T), when(X, P).
usage(T, P) :- where(X, T), when(X, P).

% PART C
conflict(C1, C2) :- where(C1, X), where(C2, X), when(C1,Y), when(C2,Y), C1\=C2.

% PART D
meet(S1, S2) :- enroll(S1,X), enroll(S2,Y), where(X,P), where(Y,P), when(X,T), when(Y,T), S1\=S2, X=:=Y.
meet(S1, S2) :- enroll(S1,X), enroll(S2,Y), where(X,P), where(Y,P), when(X,T1), when(Y,T2), T2+1=:=T1, S1\=S2, not(conflict(X, Y)).
meet(S1, S2) :- enroll(S1,X), enroll(S2,Y), where(X,P), where(Y,P), when(X,T1), when(Y,T2), T2-1=:=T1, S1\=S2, not(conflict(X, Y)).

/* Exercise 2 */
% PART A
% An empty list returns empty
rdup([], M) :- append([], [], M), !.
% A list of one element has no duplicates
rdup([L1|L2], M) :- L2=[], append([L1], [], M), !.
% If the head is a member, check the rest of the list and bind to M
rdup([L1|L2], M) :- member(L1,L2), !, rdup(L2,M).
% If the head is not a member, check the rest of the list and append it to the head.
rdup([L1|L2], M) :- not(member(L1,L2)), !, rdup(L2,X), append([L1],X,M).

% PART B
% An empty list is flat
flat([],M) :- !, append([], [], M).
% Flatten this element, flatten the rest, append the flattened list to F
flat([L1|L2],F) :- flat(L1,F1), flat(L2,F2), append(F1, F2, F).
% If the head is not a list(empty or otherwise), continue to flatten the rest
flat([L1|L2],[L1|F]) :- L1 \= [], L1 \= [_|_], flat(L2,F).


% PART C
% Helper predicate for project
% LIST, INDEX, RETURN VAL
getEl([], _, _) :- !.
% If the value is 1, we are at the desired element. Bind to N
getEl([L1|_], X, N) :- N=L1, X=:=1.
% Otherwise, X = X-1 and we search the rest of the list
getEl([_|L2], X, N) :- L2\=[], X1 is X-1, getEl(L2,X1, N), X\=1.


% Empty values is empty result
project(_,[],P) :- append([], [], P), !.
% Empty list cannot be projected
project([],_,P) :- !, append([], [], P).
% This is the end of the list, append it by itself to P
project([N1|N2], L, P) :- N2=[], getEl(L, N1, X), append([X], [], P).
% This is the end of the list. It is out-of-bounds. P is empty. X is an empty dummy.
project([N1|N2], L, P) :- N2=[], not(getEl(L, N1, X)), append([], [], P), X=X.
% Get element N1 from list L, bind to X, project the rest of the list
project([N1|N2], L, P) :- N2\=[], getEl(L,N1,X), project(N2, L, Y), append([X], Y, P).
% This is NOT the end of the list. N1 is out-of-bounds. Project the rest, and append the rest to P, but not this.
% X is an empty dummy.
project([N1|N2], L, P) :- N2\=[], not(getEl(L,N1,X)), project(N2, L, Y), append([], Y, P), X=X.