
end :- true.
else :- true.

countElementsInList(_, [],  0) :- !.
countElementsInList(Element, [Element | Rest], Cant) :-  !,
    countElementsInList(Element, Rest, Cant2),
    Cant is Cant2 + 1.
countElementsInList(Element, [First | Rest], Cant) :- 
    not(Element = First),
    !,
    countElementsInList(Element, Rest, Cant).

% One-base Indexing
elementAt([Element | _], 1, Element) :- !.

elementAt([_ | Rest], Position, Element) :- 
    NewPos is Position - 1,
    elementAt(Rest, NewPos, Element).

removeElementAt([Element | List], 1, Element, List) :- !.

% Puts in Element the element at Position from List, and stores in Ou the rest of the list.
removeElementAt([X | List], Position, Element, [X | Ou]) :-
    NewPos is Position - 1,
    removeElementAt(List, NewPos, Element, Ou).

% Insert into List the element Element at Position and put the result in Ou.
insertElementAt(List, 1, Element, [Element | List]) :- !.
insertElementAt([X | List], Position, Element, [X | Ou]) :-
    NewPos is Position - 1,
    insertElementAt(List, NewPos, Element,  Ou).


% Remove duplicates
makeSet(List, Set) :-
    setof(X,
        member(X,List),
        Set
    );
    Set = [],
    true.

% Create a list from the repetition of an element
timesCreateList(_, 0, []) :- !.
timesCreateList(Element, Times, [Element | List]) :-
    T2 is Times - 1,
    timesCreateList(Element, T2, List) .

    
% Simulates a if - else sentence
if_else(C1, B1, _, _) :-
    call(C1),
    !,
    call(B1).
if_else(_, _, C2, B2) :-
    call(C2),
    !,
    call(B2).


% Create a list with the integers from L to R.
rangeLR(N, N, [N]) :- !.
rangeLR(L, R, [ L | List]) :-
    L2 is L + 1,
    rangeLR(L2, R, List).

%count elements in a list
count([], 0).
count([_ | L], C) :- count(L, C1), C is C1 + 1. 


%Compare two sorted lists
compareSortedLists([], [], equals) :- !.

compareSortedLists([L | L1], [R | L2], Result) :-
    if_else(
        L == R,
            compareSortedLists(L1, L2, Result),
        true,
            Result = non_equals
    ),
    true.



%Compare two lists as multisets
compareLists(L1, L2, Result) :- 
    sort(L1, SL1),
    sort(L2, SL2),
    length(L1, Length1),
    length(L2, Length2),
    if_else(
        Length1 = Length2,
            compareSortedLists(SL1,SL2, Result),
        true,
        Result = non_equals
    ),
    true.

%retract a predicate and update it with new arguments
update(Functor, Params_Old, Params_New) :-
    Old =.. [Functor | Params_Old],
    New =.. [Functor | Params_New],
    retract(Old),
    assert(New).

%retract a with a single argument, and update it incrementing it by a unit
increment(T) :- 
    T =.. [Functor | [X]],
    call(T),
    X1 is X + 1,
    update(Functor, [X], [X1]).

%rSame, but return previous and after values
increment(T, X, X1) :- 
    T =.. [Functor | [X]],
    call(T),
    X1 is X + 1,
    update(Functor, [X], [X1]).


%retract a with a single argument, and update it decreasing it by a unit
decrement(T) :-
    T =.. [Functor | [X]],
    call(T),
    X1 is X - 1,
    update(Functor, [X], [X1]).

%Same, but return previous and after values
decrement(T, X, X1) :-
    T =.. [Functor | [X]],
    call(T),
    X1 is X - 1,
    update(Functor, [X], [X1]).



%We cannot write in the output anymore
stopPrinting(ROut) :- 
    open_null_stream(Out),
    current_output(ROut),
    tell(Out),
    true.

%If we pass the output stram as an argument, we reset it as the default stream to write
restartPrinting(ROut) :-
    current_output(Out),
    close(Out),
    tell(ROut),
    true.

%calls a predicate without writting anything to the output
noPrintCall(Functor, Args) :-
    open_null_stream(Out),
    T=.. [Functor | Args],
    with_output_to(Out, T),
    close(Out),
    
    true.
    
%calls a method once, and return true always
try(T) :-
    call(T), false;
    end.


    

