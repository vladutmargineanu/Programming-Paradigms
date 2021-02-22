min(X, Y, M) :- X =< Y, M is X.
min(X, Y, M) :- X > Y,  M is Y.

min2(X, Y, M) :- X =< Y, M = X.
min2(X, Y, M) :- X > Y,  M = Y.

% Echivalent cu min2.
min3(X, Y, X) :- X =< Y.
min3(X, Y, Y) :- X > Y.

% Gresit!
min4(X, Y, X) :- X =< Y.
min4(X, Y, Y).

min5(X, Y, X) :- X =< Y, !.
min5(X, Y, Y).