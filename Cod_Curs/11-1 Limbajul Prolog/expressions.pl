:- include('permutations.pl').

% expr(+List, +Result, ?Expression)
% Expression este o expresie simbolica, eventual parantezata,
% construita cu numerele din List, astfel incat valoarea ei sa fie Result.
expr(L, R, E) :- perm(L, P), arrange(P, E), E =:= R.

% arrange(+List, ?Expression)
% Expression este o expresie simbolica, eventual parantezata,
% construita cu numerele din Lista, in ordinea data.
arrange([X], X) :- !.
arrange(L, (EA) + (EB)) :- app1(A, B, L), arrange(A, EA), arrange(B, EB).
arrange(L, (EA) - (EB)) :- app1(A, B, L), arrange(A, EA), arrange(B, EB).
arrange(L, (EA) * (EB)) :- app1(A, B, L), arrange(A, EA), arrange(B, EB).
arrange(L, (EA) / (EB)) :- app1(A, B, L), arrange(A, EA), arrange(B, EB), EB =\= 0.

% app1(List1, List2, List)
% List1 rezulta din concatenarea listelor List1 si List2, AMBELE fiind NEVIDE!
app1(L1, L2, L) :- append(L1, L2, L), L1 \= [], L2 \= [].