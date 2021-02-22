% append(L1, L2, Res)
append([], L, L).
append([H|T], L, [H|Res]) :- append(T, L, Res).
