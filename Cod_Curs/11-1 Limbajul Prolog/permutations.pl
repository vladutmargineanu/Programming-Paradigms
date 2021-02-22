% del(Element, List, RemainingList)
% RemainingList este lista rezultata prin eliminarea lui Element
% din List, in orice mod.
del(X, [X | T], T).
del(X, [H | T], [H | R]) :- del(X, T, R).

% perm(List, Permutation)
% Permutation este o permutare a lui List.
perm([], []).
perm(L, [X | P]) :- del(X, L, R), perm(R, P).