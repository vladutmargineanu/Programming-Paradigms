% Alice came across a lion and a unicorn in a forest of forgetfulness.
% Those two are strange beings.
% The lion lies every Monday, Tuesday and Wednesday
%	and the other days he speaks the truth.
% The unicorn lies on Thursdays, Fridays and Saturdays,
%	however the other days of the week he speaks the truth.
% Lion: Yesterday I was lying.
% Unicorn: So was I.
% Which day did they say that?

yesterday(mon, sun).
yesterday(tue, mon).
yesterday(wed, tue).
yesterday(thu, wed).
yesterday(fri, thu).
yesterday(sat, fri).
yesterday(sun, sat).

lies(lion, [mon, tue, wed]).
lies(unicorn, [thu, fri, sat]).

says(Animal, Today) :-
    lies(Animal, Days),
    yesterday(Today, Yesterday),
    ((member(Today, Days), \+ member(Yesterday, Days)) ;
     (\+ member(Today, Days), member(Yesterday, Days))).
    
sol(Today) :- says(lion, Today), says(unicorn, Today).