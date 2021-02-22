minList1 [h]     = h
minList1 (h : t) = min h (minList1 t)

minList2 (h : t) = foldl min h t 