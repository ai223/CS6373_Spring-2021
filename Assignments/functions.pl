-- Q1 (Write a program that finds the maximum of a simple list of numbers.)

maxm([X], M) :- M is X.

maxm([X|XS], M) :-
    maxm(XS, E),
    max(X, E, D),
    M is D.

max(A, B, R) :- A >= B, R is A.
max(A, B, R) :- A < B, R is B.

-- Q2 (Write a program that succeeds if the intersection of two given list parameters is empty.)

noIntersection([], _).
noIntersection([X|XS], L2) :-
    \+(inlist(X, L2)), noIntersection(XS, L2).

inlist(X, [X|_]).
inlist(X, [_|XS]) :- inlist(X, XS).

-- Q3 (Write a program that returns a list containing the union of the elements of two given lists.
-- Assume the list represents sets (i.e. unique elements) and the union returns unique elements.)

union([],[],[]).
union(L1,[],L1).
union([X|XS], L2, R) :-
    inlist(X, L2), union(L2,XS,R).
union([X|XS], L2, [X|R]):-
    \+(inlist(X,L2)), union(L2,XS,R).

inlist(X, [X|_]).
inlist(X, [_|XS]) :- inlist(X, XS).

-- Q4 (Write a program that returns the final element of a list)

lst([X], E) :- E is X.
lst([_|XS], E) :- lst(XS, E).
