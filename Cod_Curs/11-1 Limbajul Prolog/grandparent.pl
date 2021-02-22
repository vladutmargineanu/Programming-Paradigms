% constante -> litera mica
parent(andrei, bogdan).
parent(andrei, bianca).
parent(bogdan, cristi).

% variabile -> litera mare
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
