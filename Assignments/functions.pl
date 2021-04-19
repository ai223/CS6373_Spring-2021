-- Q1 (Write a program that finds the maximum of a simple list of numbers.)

maxm([X], M) :- M is X.

maxm([X|XS], M) :-
    maxm(XS, E),
    max(X, E, D),
    M is D.

max(A, B, R) :- A >= B, R is A.
max(A, B, R) :- A < B, R is B.

-- Q2 (Write a program that succeeds if the intersection of two given list parameters is empty.)

# TODO

-- Q3 (Write a program that returns a list containing the union of the elements of two given lists.
-- Assume the list represents sets (i.e. unique elements) and the union returns unique elements.)

# TODO

-- Q4 (Write a program that returns the final element of a list)

lst([X], E) :- E is X.
lst([_|XS], E) :- lst(XS, E).
