% Albert and Bernard have just met Cheryl.
% “When is your birthday?” Albert asked Cheryl.
% Cheryl thought for a moment and said, “I won’t tell you,
% but I’ll give you some clues”. She wrote down a list of ten dates:

% May 15, May 16, May 19
% June 17, June 18
% July 14, July 16
% August 14, August 15, August 17

% “One of these is my birthday,” she said.

% Cheryl whispered in Albert’s ear the month, and only the month, of her birthday.
% To Bernard, she whispered the day, and only the day.
% “Can you figure it out now?” she asked Albert.

% Albert: “I don’t know when your birthday is, but I know Bernard doesn’t know, either.”
% Bernard: “I didn’t know originally, but now I do.”
% Albert: “Well, now I know, too!”

% When is Cheryl’s birthday?

candidate(Month, Day) :-
    member(Month/Day, [ may/15, may/16, may/19
                      , june/17, june/18
                      , july/14, july/16
                      , aug/14, aug/15, aug/17
                      ]).

s1(Month, Day) :-
    candidate(Month, Day),
    % Albert: “I don’t know when your birthday is ...”
    several(candidate(Month, _)),
    % “... but I know Bernard doesn’t know, either.”
    forall(candidate(Month, D), several(candidate(_, D))).

% Bernard: “I didn’t know originally, but now I do.”    
s2(Month, Day) :-
    candidate(Month, Day),
    unique(M, s1(M, Day), Month).

% Albert: “Well, now I know, too!”
s3(Month, Day) :-
    candidate(Month, Day),
    unique(D, s2(Month, D), Day).

% Checks whether P is satisfied at least twice.
several(P) :- findall(_, P, [_, _ | _]).

% Checks whether P(X) is satisfied exactly once, and returns the unique binding
% of X in R.
unique(X, P, R) :- findall(X, P, [R]).