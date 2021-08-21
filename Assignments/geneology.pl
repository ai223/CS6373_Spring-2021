Mother relationship:
Mother(X, Y) :- parent(X, Y), female(X)

Sister relationship:
We define mother and father first:
Mother(X, Y) :- parent(X, Y), female(X)
Father(X, Y) :- parent(X, Y), male(X)
sister(X, Y) :-     female(X),
                            mother(M, X),
                            mother(M, Y),
                            father(F, X),
                            father(F, Y),
                            X /= Y
