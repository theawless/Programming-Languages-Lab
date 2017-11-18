% let facts be dynamic
:- (dynamic first/1).
:- (dynamic second/1).


% research scholars' first list (roll, name, email, type) facts
first([1, abhinav, abhinav2014, regular]).
first([2, abhishek, abhishek2014, parttime]).
first([3, yash, yashp, foreign]).
first([4, vikas, vikasyadav, qip]).

% research scholars' second list (roll, name, type, supervisor, co-supervisor(optional)) facts
second([1, abhinav, regular, profsamit]).
second([2, abhishek, parttime, profranbir, profinkulu]).
second([3, yash, foreign, profawekar]).
second([4, vikas, qip, profdas, profsaswata]).


% combine first argument(first list) and second argument(second list) to find third argument(full details)
combine_lists_([Roll, Name, Email|Type], [_, _, _|Professors], FullDetails) :-
    append([Roll, Name, Email|Professors], Type, FullDetails).

% take from first argument(first/second list) the second argument(roll)
roll_([Roll|_], Roll).

% take from first argument(first/second list) the second argument(name)
name_([_, Name|_], Name).

% prints a table for first argument(full details with cosupervisor missing)
pretty_(FullDetails) :-
    [_, _, _, _, _]=FullDetails,
    Header=[name, roll, email, supervisor, type],
    format("|~t~a~t~20||~t~a~t~40||~t~a~t~60||~t~a~t~80||~t~a~t~80||~n", Header),
    format("|~t~a~t~20||~t~a~t~40||~t~a~t~60||~t~a~t~80||~t~a~t~80||~n", FullDetails).

% prints a table for first argument(full details with cosupervisor present)
pretty_(FullDetails) :-
    [_, _, _, _, _, _]=FullDetails,
    Header=[name, roll, email, supervisor, cosupervisor, type],
    format("|~t~a~t~20||~t~a~t~40||~t~a~t~60||~t~a~t~80||~t~a~t~100||~t~a~t~80||~n", Header),
    format("|~t~a~t~20||~t~a~t~40||~t~a~t~60||~t~a~t~80||~t~a~t~100||~t~a~t~80||~n", FullDetails).

% search for first argument(roll) to find the second argument(full details of reseach scholar)
% unify with details such that both of the lists have same roll
search(Roll) :-
    integer(Roll),
    first(FirstList),
    second(SecondList),
    roll_(FirstList, RollInFirstList),
    roll_(SecondList, RollInSecondList),
    Roll==RollInFirstList,
    Roll==RollInSecondList,
    combine_lists_(FirstList, SecondList, FullDetails),
    pretty_(FullDetails).

% search for first argument(name) to find the second argument(full details of reseach scholar)
% unify with details such that both of the lists have same name
search(Name) :-
    atom(Name),
    first(FirstList),
    second(SecondList),
    name_(FirstList, NameInFirstList),
    name_(SecondList, NameInSecondList),
    Name==NameInFirstList,
    Name==NameInSecondList,
    combine_lists_(FirstList, SecondList, FullDetails),
    pretty_(FullDetails).