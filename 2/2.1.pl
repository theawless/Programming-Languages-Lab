% let facts be dynamic
:- (dynamic parent/2).
:- (dynamic male/1).
:- (dynamic female/1).


% parent relationship facts
parent(jatin, avantika).
parent(jolly, jatin).
parent(jolly, kattappa).
parent(manisha, avantika).
parent(manisha, shivkami).
parent(bahubali, shivkami).

% gender facts
male(kattappa).
male(jolly).
male(bahubali).
female(shivkami).
female(avantika).


% check if first argument(person) is the uncle of second argument(person)
% atleast one of uncle's parent and nephew's grandparent should be same
uncle(Uncle, Nephew) :-
    male(Uncle),
    parent(ParentOfNephew, Nephew),
    parent(CommonGrandParent, ParentOfNephew),
    parent(CommonGrandParent, Uncle),
    not(Uncle==ParentOfNephew).    

% check if first argument(person) is the halfsister of second argument(person)
% only one of the parents of the halfsister and halfsibling should be same
halfsister(HalfSister, HalfSibling) :-
    female(HalfSister),
    parent(CommonParent, HalfSister),
    parent(CommonParent, HalfSibling),
    parent(ParentOfHalfSister, HalfSister),
    parent(ParentofHalfSibling, HalfSibling),
    not(ParentOfHalfSister==CommonParent),
    not(ParentofHalfSibling==CommonParent),
    not(ParentOfHalfSister==ParentofHalfSibling).