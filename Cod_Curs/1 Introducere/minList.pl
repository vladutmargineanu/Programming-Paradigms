min(X, Y, X) :- X =< Y.
min(X, Y, Y) :- Y < X.

minList([M], M).
minList([X, Y | T], M) :-
    minList([Y | T], N), min(X, N, M).
