% let facts be dynamic
:- (dynamic max_path_length/1).
:- (dynamic total_row/1).
:- (dynamic total_column/1).
:- (dynamic start_points/1).
:- (dynamic end_points/1).
:- (dynamic walls_up/1).
:- (dynamic walls_right/1).
:- (dynamic walls_down/1).
:- (dynamic walls_left/1).


% grid dimension facts
total_row(6).
total_column(6).

% grid point facts
start_points([1, 13, 25]).
end_points([12, 24, 36]).

% grid walls facts
walls_up([1, 2, 3, 4, 5, 6, 15, 16, 20, 21, 24, 29, 33, 34]).
walls_right([2, 6, 8, 12, 16, 18, 20, 23, 24, 26, 28, 30, 36]).
walls_down([9, 10, 14, 15, 18, 23, 27, 28, 31, 32, 33, 34, 35, 36]).
walls_left([1, 3, 7, 9, 13, 17, 19, 21, 24, 25, 27, 29, 31]).


% finds the max path length
max_path_length(MaxPathLength) :-
    total_row(TotalRow),
    total_column(TotalColumn),
    MaxPathLength is TotalRow*TotalColumn.

% move the current point up (i.e. subtract total columns) and add it to the current path
move_up_(CurrentPoint, EndPoint, CurrentPathLength, CurrentPath, FinalPathLength, FinalPath) :-
    walls_up(Walls),
    \+ member(CurrentPoint, Walls),
    total_column(TotalColumn),
    NewCurrentPoint is CurrentPoint-TotalColumn,
    \+ member(NewCurrentPoint, CurrentPath),
    NewCurrentPathLength is CurrentPathLength+1,
    append(CurrentPath, [NewCurrentPoint], NewCurrentPath),
    move_(NewCurrentPoint, EndPoint, NewCurrentPathLength, NewCurrentPath, FinalPathLength, FinalPath).

% move the current point right (i.e. add one) and add it to the current path
move_right_(CurrentPoint, EndPoint, CurrentPathLength, CurrentPath, FinalPathLength, FinalPath) :-
    walls_right(Walls),
    \+ member(CurrentPoint, Walls),
    NewCurrentPoint is CurrentPoint+1,
    \+ member(NewCurrentPoint, CurrentPath),
    NewCurrentPathLength is CurrentPathLength+1,
    append(CurrentPath, [NewCurrentPoint], NewCurrentPath),
    move_(NewCurrentPoint, EndPoint, NewCurrentPathLength, NewCurrentPath, FinalPathLength, FinalPath).

% move the current point down (i.e. add total columns) and add it to the current path
move_down_(CurrentPoint, EndPoint, CurrentPathLength, CurrentPath, FinalPathLength, FinalPath) :-
    walls_down(Walls),
    \+ member(CurrentPoint, Walls),
    total_column(TotalColumn),
    NewCurrentPoint is CurrentPoint+TotalColumn,
    \+ member(NewCurrentPoint, CurrentPath),
    NewCurrentPathLength is CurrentPathLength+1,
    append(CurrentPath, [NewCurrentPoint], NewCurrentPath),
    move_(NewCurrentPoint, EndPoint, NewCurrentPathLength, NewCurrentPath, FinalPathLength, FinalPath).

% move the current point left (i.e. subtract one) and add it to the current path
move_left_(CurrentPoint, EndPoint, CurrentPathLength, CurrentPath, FinalPathLength, FinalPath) :-
    walls_left(Walls),
    \+ member(CurrentPoint, Walls),
    NewCurrentPoint is CurrentPoint-1,
    \+ member(NewCurrentPoint, CurrentPath),
    NewCurrentPathLength is CurrentPathLength+1,
    append(CurrentPath, [NewCurrentPoint], NewCurrentPath),
    move_(NewCurrentPoint, EndPoint, NewCurrentPathLength, NewCurrentPath, FinalPathLength, FinalPath).

% if the current path length is greater than required final path length, then cut and fail this branch
move_(_, _, CurrentPathLength, _, FinalPathLength, _) :-
    CurrentPathLength>FinalPathLength, !,
    false.

% if current point is the required end point and current path length is equal to required path length
% then save this path, cut and success this branch
move_(CurrentPoint, EndPoint, CurrentPathLength, CurrentPath, FinalPathLength, FinalPath) :-
    CurrentPoint==EndPoint,
    CurrentPathLength==FinalPathLength,
    FinalPath=CurrentPath.

% try to move in all directions
move_(CurrentPoint, EndPoint, CurrentPathLength, CurrentPath, FinalPathLength, FinalPath) :-
    (   move_up_(CurrentPoint, EndPoint, CurrentPathLength, CurrentPath, FinalPathLength, FinalPath)
    ;   move_right_(CurrentPoint, EndPoint, CurrentPathLength, CurrentPath, FinalPathLength, FinalPath)
    ;   move_down_(CurrentPoint, EndPoint, CurrentPathLength, CurrentPath, FinalPathLength, FinalPath)
    ;   move_left_(CurrentPoint, EndPoint, CurrentPathLength, CurrentPath, FinalPathLength, FinalPath)
    ).

% try to find paths between start point and end point of given length else increase length and repeat
min_path_between_(StartPoint, EndPoint, FinalPathLength, FinalPath) :-
    max_path_length(MaxFinalPathLength),
    numlist(1, MaxFinalPathLength, FinalPathLengths),
    member(FinalPathLength, FinalPathLengths),
    path(StartPoint, EndPoint, FinalPathLength, FinalPath), !.


% check if first argument(start point) ever takes us to second argument(end point)
% within third argument(path length) through fourth argument(finalpath)
path(StartPoint, EndPoint, FinalPathLength, FinalPath) :-
    max_path_length(MaxFinalPathLength),
    numlist(1, MaxFinalPathLength, FinalPathLengths),
    member(FinalPathLength, FinalPathLengths),
    start_points(StartPoints),
    member(StartPoint, StartPoints),
    end_points(EndPoints),
    member(EndPoint, EndPoints),
    move_(StartPoint, EndPoint, 1, [StartPoint], FinalPathLength, FinalPath).

% find min paths between start point and end point
min_path(StartPoint, EndPoint, FinalPathLength, FinalPath) :-
    start_points(StartPoints),
    member(StartPoint, StartPoints),
    end_points(EndPoints),
    member(EndPoint, EndPoints),
    min_path_between_(StartPoint, EndPoint, FinalPathLength, FinalPath).