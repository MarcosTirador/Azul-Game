
% :- module(create, [createPlayers/1]).

:- [wall, tools].
:- dynamic preparationBoard/6, wall/2, player/2, bag/1, factory/2, discardPile/1, center/2, preparationBoard/7.

% player(Id, Score)
% This predicate saves the Id of a player, together with his score.
% The first one isn't more than is index in the player list or position in the table.
% The second one is clear from the name

% bag(B)
% This predicate is used to know when a list B is the Bag of the game.

% factory(Id, Items)
% Id contains the order of the factory, or position.
% Items is a list where each element represent a color, this element can be 'black', 'white', 'yellow', 'red', or 'blue' and any other violates the purpose of this implementation.

% center(Special, Tiles)
% Special is 'special' or 'non_special' depending of wheter the center contains the starting player mark or don't. 
% Tiles is a list containing Colors, representing each one of the colors in the center.

% discardPile(B)
% Same as in bag(B).
% The discard pile is considered the tiles that are left over the lid, when a slab is putted into the wall. 

% wall(Id, Tiles)
% Each wall is unique for each player, and it contains the id of its owner, and the tiles that him has already putted. 
% This last, is represented as a list of tuples of the form (Row, Color), indicating that him put already the slab of color Color in the row Row, as here fits only one. 

% preparationBoard(PlayerId,R1,R2,R3,R4,R5,Fl)
% Represents the Pattern lines and the floo line of each player.
% What PlayerId stands for is obvious now. 
% Ri reffers to the ith pattern line, and it storage it as a tuple of the 
% form (Color, Amount, Max), where Color represents the color of the tiles already in it and it has to be one of  'black', 'white', 'yellow', 'red', 'blue'; or 'blank' if no tiles here jet.
% Amount is the Amount of tiles of the previous color in the row (0 if 'blank'), and Max is the limit of tiles that fit into this row, which can also be used to identified to which row are we refeering to, as an index, in other contexts. 
%Fl refeers to the Floor Line and it is a tuple of the form (Special, Amount, Max), where Max is always 7, which appart from taking note of how many tiles we can put in here, also can be used to identify this row from others.
% Special can be either 'special' or 'non_special' and it is the way that we have to know if the starting player maker is in this FL.
% Finally Amount stands for the same as before 

negativeScore(0, 0).
negativeScore(1, -1).
negativeScore(2, -2).
negativeScore(3, -4).
negativeScore(4, -6).
negativeScore(5, -8).
negativeScore(6, -11).
negativeScore(7, -14).

% Relation No Players - No Factories
    rNFNP(2, 5).
    rNFNP(3, 7).
    rNFNP(4, 9).


% Creating the discard pile
    createDiscardPile(_) :- 
        assert(discardPile([])).

% Creating the center
    createCenter(_) :-
        assert(center(special, [])).

    resetCenter(_) :-
        retractall(center(_, _)),
        assert(center(special, [])),
        true.


% Fill Bag
    put(Color, 1, Lb, La) :- 
        La = [Color | Lb],
        !.
    put(Color, N, Lb, [Color | La]) :-
        N1 is N - 1,
        put(Color, N1, Lb, La).

    resetBag(_) :-
        retractall(bag),
        put(yellow, 20, [], La),
        put(red, 20, La, La2),
        put(blue, 20,La2, La3),
        put(black, 20, La3, La4),
        put(white, 20, La4, LF),
        assert(bag(LF)),
        true.

    reFillBag(_) :-
        retract(bag(_)),
        discardPile(DP),
        assert(bag(DP)),
        retract(discardPile(DP)),
        assert(discardPile([])).


% Create Players
    createPlayer(Id) :- 
        assert(player(Id, 0)),
        assert(wall(Id, [])),
        assert(preparationBoard(Id, (blank, 0, 1), (blank, 0, 2),(blank, 0, 3),(blank, 0, 4), (blank, 0, 5), (non_special, 0, 7))).

    createPlayers(0) :-  !.

    createPlayers(N) :- N1 is N - 1, createPlayers(N1), createPlayer(N).
%

%Create the factories
    createFactory(Id) :- 
        assert(factory(Id, [])).

    createFactories(0) :- !.

    createFactories(N) :-  N1 is N - 1, createFactories(N1), createFactory(N).
%

%Filling the Factories
    pickFactoryTiles(_, 0, L, L) :- !.

    pickFactoryTiles(IdFact, Cant, Lb, La) :-
        bag(B),
        length(B,Length),
        Length > 0,
        !,
        N1 is Cant - 1,
        
        random_select(Color, B, B1),
        retract(bag(B)),
        assert(bag(B1)),

        pickFactoryTiles(IdFact, N1, Lb, La1),
        append(La1, [Color], La),
        !.
    pickFactoryTiles(IdFact, Cant, Lb, La) :- %If there are no tiles left try to refill it with the tiles on the lid.
        reFillBag(_),
        bag(B),
        length(B,Length),
        Length > 0,
        !,
        pickFactoryTiles(IdFact, Cant, Lb, La).

    % pickFactoryTiles(_, _, Lb, Lb) :- !. %If even we try to refill but still nothing...the factory remains empty.

    pickAllFactoriesTiles(0) :- !.

    pickAllFactoriesTiles(NoFactories) :- 
        N1 is NoFactories - 1,
        pickAllFactoriesTiles(N1),
        IdFact = NoFactories,
        
        pickFactoryTiles(IdFact, 4, [], NewTiles),
        factory(IdFact, Items),
        retract(factory(IdFact, Items)),
        assert(factory(IdFact, NewTiles)).
%

% Printing Status

    %Print the status of the table except for the boards.
    printStatus(_) :-   
        format('The Table in this moment:~n'),
        printPlayerStatus(_),
        printFactoryStatus(_),
        printBagStatus(_),
        printDiscardPileStatus(_),
        printCenterStatus(_),
        format('~n'),
        !.

    printCenterStatus(_) :-
        center(Special, List),
        length(List, LL),
        (
            Special == special,
            Length is LL + 1,
                SS = [special]
            ;not(Special == special),
            Length is LL,
                SS = []
        ),
        append(SS, List, ToShow),
        format('The Center has ~a elements which are: ~n ~w~n', [Length, ToShow]).

    printBagStatus(_) :-
        bag(B),
        length(B, Length),
        format('The Bag has ~a elements. ~n', [Length]).

    printDiscardPileStatus(_) :-
        discardPile(DP),
        length(DP, Length),
        format('There are ~a elements on the lid. ~n', [Length]).

    printFactoryStatus(_) :-
        findall((FactId, Items),
            (
                factory(FactId, Items)
            ),
            List
        ),
        sort(1,@=<, List, SList),
        findall(_,
            (
                member((FactId, Items),SList),
                format('The factory #~a has the elements: ~w~n', [FactId, Items])
            ),
            _
        ).

    printPlayerStatus(_) :-
        findall((PlayerId, Score),
            (
                player(PlayerId, Score)
            ),
            List
        ),
        sort(1,@=<, List, SList),
        findall(_,
            (
                member((PlayerId, Score),SList),
                format('The player #~a has a score of: ~a~n', [PlayerId, Score])
            ),
            _
        ).

    %To Show in the board
        toShowColor(red, 'R').
        toShowColor(blank, '_').
        toShowColor(blue, 'B').
        toShowColor(black, 'D').
        toShowColor(white, 'W').
        toShowColor(yellow, 'Y').

    %To Show in the wall, as a guide representing which piece goes where
        toShowColorLower(red, 'r').
        toShowColorLower(blue, 'b').
        toShowColorLower(black, 'd').
        toShowColorLower(white, 'w').
        toShowColorLower(yellow, 'y').

    %Prints a row of the board of one player.
    printRow(WallTiles, R, GuideWall) :-
        R = (Color, Amount, Max),
        EmptyTimes is 5 - Max,
        BlankTimes is Max - Amount,
        timesCreateList(' ', EmptyTimes,Empty),
        timesCreateList('_', BlankTimes,Blank),
        
        toShowColor(Color, ColorSymbol),
        timesCreateList(ColorSymbol, Amount, Fill),
        append(Empty, Blank, O1),
        append(O1, Fill, Ou),


        printRowWall(WallTiles, Max, MyWallRow), 
        printRowWall(GuideWall, Max, GuideWallRow), 

        append(MyWallRow, GuideWallRow, PrintingWallRow),
        append(Ou, PrintingWallRow, OuJoin),

        
        format('~a ~a ~a ~a ~a  |  ~a ~a ~a ~a ~a  |  ~a ~a ~a ~a ~a~n',OuJoin),
        % format('~c ~c ~c ~c ~c ~n',Ou),
        !.

    %Prints the Floor Line of one player.
    printFloorLine(Fl) :-
        Fl = (Special, Amount, 7),
        (
            Special == special,
                C = '1'
            ;not(Special == special),
                Amount > 0,
                C = 'X'
            ;not(Special == special),
                Amount == 0,
                C = '_'
        ),
        FAm is max(0,Amount - 1),
        Am is 7 - 1 - FAm,

        timesCreateList('X', FAm, Fill),
        timesCreateList('_', Am, Rest),

        append([C],Fill,Ou1),
        append(Ou1, Rest, Ou),
        format('Floor Line:~n ~a ~a ~a ~a ~a ~a ~a ~n',Ou).


    %Print the Board of One Player
    printPlayerPatternLines(PlayerId) :-
        preparationBoard(PlayerId, R1, R2, R3, R4, R5, Fl),
        format('Board Corresponding To: Player # ~a~n', [PlayerId]),
        format('Rows       |  Wall       |  Guide Wall   ~n~n'),
        makeGuideWall(GuideWall),
        findall(_,
            (
                member(R, [R1,R2, R3, R4, R5]),
                wall(PlayerId, Wall),
                printRow(Wall, R, GuideWall)
            ),
            _
        ),
        printFloorLine(Fl),
        format('~n').

    %Print the Board of all of the Players.
    printAllPlayersBoards(NoPlayers) :-
        rangeLR(1, NoPlayers, List),
        findall(_,(member(X,List),printPlayerPatternLines(X)),_).




%

%Obtain Info
    searchSpecificColor(Color, 0, L, R) :- !,
        center(_, LCenter),
        countElementsInList(Color, LCenter, C),
        (
            C > 0,
                append(L, [(Color, 10, C)], R)
            ;C == 0,
                R = L
        ).
    searchSpecificColor(Color, IdFactory, L, R) :-
        factory(IdFactory, LFact),
        countElementsInList(Color, LFact, C),
        (
            C > 0,
                append(L, [(Color, IdFactory, C)], R2)
            ;C == 0,
                R2 = L
        ),
        N1 is IdFactory - 1,
        searchSpecificColor(Color, N1, R2, R).
        
    searchByColor([], _, L, L) :-
        !.
    searchByColor([Color | Rest], NoFactories, L, R) :-
        searchByColor(Rest, NoFactories, L, R2),
        searchSpecificColor(Color, NoFactories, R2, R).

    %Give a list of all the availables moves for the next player to make. A move comes in a tuple
    % (R, F, A) which means pick A tiles of color R from factory F, where F = 10 if it referes to the center
    obtainAvailableMoves(NoFactories, AvailableMoves) :-
        findall(Color, color(Color), Colors),
        searchByColor(Colors,NoFactories, [], AvailableMoves),
        !.



    %Filter from the available moves those who fits (are valid) to the player.
    filterValidPlayerMove(CurrentPlayer, AvailableMoves, Ou1, Ou2) :-
        preparationBoard(CurrentPlayer, R1, R2, R3, R4, R5, _),

        findall((ColorM, IdFac, AmountM, Max), %Find the ones that can go in a pattern line
            (
                member(Move, AvailableMoves),
                Move = (ColorM, IdFac, AmountM),
                member(R,[R1,R2,R3,R4,R5]),
                R = (ColorR, AmountR, Max),
                (ColorR == ColorM; (ColorR == blank, colorMissingInRowInWall(CurrentPlayer, Max, CMR), member(ColorM, CMR))),
                Max >= AmountR + 1
            ),
            Ou11
        ),
        makeSet(Ou11,Ou1), %makes a set from a list
        findall((ColorM, IdFac, AmountM, 7),%Find those that go directly to the floor line
            (
                member(Move, AvailableMoves),
                Move = (ColorM, IdFac, AmountM)
            ),
            Ou21
        ),
        makeSet(Ou21,Ou2),
        !.
%

%Perform a move
    %Actualize the common part of the table with the last move action.
    removeTilesFromTable(Color, IdFac, Special) :-
        
        IdFac == 10, %is the center
            center(Special, List),
            findall(Tile,
                (
                    member(Tile,List),
                    not(Tile == Color)
                ),
                NewCenter
            ),
            retractall(center(_,_)),
            assert(center(non_special, NewCenter))
        ; IdFac =\= 10, %it is just a factory 
            factory(IdFac, Items),
            center(SpecialC, CenterItems),
            findall(X,
                (
                    member(X, Items),
                    not(X == Color)
                ),
                AddCenter
            ),
            append(CenterItems,AddCenter,NewCenter),
            retractall(center(_,_)),
            assert(center(SpecialC, NewCenter)),

            retract(factory(IdFac, Items)),
            assert(factory(IdFac, [])),
            Special = non_special
        .

    addTilesToPreparationBoard(CurrentPlayer, Color, Amount, IdRow, Special) :-
        IdRow == 7, %Directly to the floor
        !,
        preparationBoard(CurrentPlayer, R1, R2, R3, R4, R5, Fl),
        Fl = (CurrentSpecial, CurrentAmount, Max),
        (
            (Special == special),
                NewSpecial = special,
                NewAmount is min(7, Amount+ CurrentAmount + 1)
            ; (CurrentSpecial == special ,  not(Special == special)),
                NewSpecial = special,
                NewAmount is min(7, Amount + CurrentAmount)
            ; (not(CurrentSpecial == special) ,  not(Special == special)),
                NewSpecial = non_special,
                NewAmount is min(7, Amount+ CurrentAmount )
        ),
        actualizeDiscardPile(Color,Amount), %We put all in the lid since here because they will be discarted before the bag would need to be refilled.
        retract(preparationBoard(CurrentPlayer, R1, R2, R3, R4, R5, Fl)),
        assert(preparationBoard(CurrentPlayer, R1, R2, R3, R4, R5, (NewSpecial, NewAmount, Max))),
        true.

    addTilesToPreparationBoard(CurrentPlayer, Color, Amount, IdRow, Special) :-
        IdRow =\= 7, %At least one to a factory
        !,
        preparationBoard(CurrentPlayer, R1, R2, R3, R4, R5, Fl),
        removeElementAt([R1,R2,R3,R4,R5],IdRow, R, LR),
        R = (_, CurrentAmount, Max),
        NewAmount is min(Max, Amount + CurrentAmount),
        NewRow = (Color, NewAmount, Max),
        insertElementAt(LR, IdRow, NewRow, NewList),
        NewList = [NR1,NR2,NR3,NR4,NR5],
        
        retract(preparationBoard(CurrentPlayer, R1, R2, R3, R4, R5, Fl)),
        assert(preparationBoard(CurrentPlayer,NR1,NR2,NR3,NR4,NR5, Fl)),
        
        %Send the rest to the floor line
        RestOfTiles is max(0, Amount + CurrentAmount - Max),
        addTilesToPreparationBoard(CurrentPlayer, Color, RestOfTiles, 7, Special).
        

%

%Finishing Round

    actualizeDiscardPile(Color, Times) :-
        discardPile(DP),
        retractall(discardPile(_)),

        timesCreateList(Color, Times, List),

        append(DP, List, NewDP),
        assert(discardPile(NewDP)),
        !.

    calculatePositiveScoreSingleRow(R, WallTiles,NewRow, NewWall, OldScore, NewScore) :-
        R = (Color, Amount, Max),
        (
            Amount == Max, %A row is filled so the tiles must go to the wall.
                (
                    NewRow = (blank, 0, Max),
                    calculateScoreByInsertingInWall(Max, Color, WallTiles, ScoreW),
                    NewScore is OldScore + ScoreW,
                    NewWall = [(Max, Color) | WallTiles],
                    AmountToDiscard is max(0, Amount - 1),
                    actualizeDiscardPile(Color, AmountToDiscard)
                )
            ; Amount =\= Max,
                NewRow = R,
                NewWall = WallTiles,
                NewScore is OldScore

        ),
        true.


    calculatePositiveScoreAllRows([], WallTiles, [], WallTiles, OldScore, OldScore) :- !.
    calculatePositiveScoreAllRows([R | Rest], WallTiles, [NewRow | NewList], NewWall, OldScore, NewScore) :-
        calculatePositiveScoreSingleRow(R, WallTiles, NewRow, NewWall_1, OldScore, NewScore1),
        calculatePositiveScoreAllRows(Rest, NewWall_1, NewList, NewWall,NewScore1,NewScore) .





    actualizePlayerScore(PlayerId, NextPlayer):-
        preparationBoard(PlayerId, R1, R2, R3, R4, R5, Fl),
        player(PlayerId, OldScore),
        wall(PlayerId, WallTiles),

        Fl = (Special, AmountFl, _),
        ( %See if this is the next player.
            Special == special,
                NextPlayer = PlayerId

            ; not(Special == special)
        ),
        negativeScore(AmountFl, NegativeScore),

        calculatePositiveScoreAllRows([R1, R2, R3, R4, R5], WallTiles, NewList, NewWall, 0, PositiveScore),
        NewScore is PositiveScore + OldScore + NegativeScore,
        NewList = [NR1, NR2, NR3, NR4, NR5],

        %Actualize the preparation board (pattern line) of this player.
        retract(preparationBoard(PlayerId, R1, R2, R3, R4, R5, Fl)),
        assert(preparationBoard(PlayerId,NR1, NR2, NR3, NR4, NR5, (non_special, 0, 7))), 

        retract(wall(PlayerId, WallTiles)),
        assert(wall(PlayerId, NewWall)),

        retract(player(PlayerId,OldScore)),
        assert(player(PlayerId, NewScore)).


    actualizeAllScores(0, _) :- !.

    actualizeAllScores(NoPlayers, NextPlayer) :-    
        actualizePlayerScore(NoPlayers, NextPlayer),
        C is NoPlayers - 1,
        actualizeAllScores(C, NextPlayer).


    checkIfPlayerEndedGame(PlayerId, Ended) :-
        wall(PlayerId, Wall),
        checkIfFullRow(Wall, Ended).


    checkTilesleft(Over) :-
        bag(B),
        discardPile(DP),
        length(B, L1),
        length(DP, L2),
        L is L1 + L2,
        if_else(
            L == 0,
                Over is 1,
            true,
                Over is 0
        ),
        true.


    checkIfGameOver(0, 0) :- !.
    
    checkIfGameOver(NoPlayers, Over) :- 
        N is NoPlayers - 1,
        checkIfGameOver(N, Over1),
        checkIfPlayerEndedGame(NoPlayers, Over2),
        checkTilesleft(Over3),
        Over is Over1 + Over2 + Over3.  

% 

%FinishingGame
    
    calculateSinglePlayerFinalScore(PlayerId) :-
        wall(PlayerId, WallTiles),
        calculateClosingScore(WallTiles, Score), %The bonus for completing rows, columns or all of one color
        player(PlayerId, OldScore),
        retract(player(PlayerId, OldScore)),
        NewScore is OldScore + Score,
        assert(player(PlayerId, NewScore)).

    calculateFinalScores(0) :- !.
    calculateFinalScores(NoPlayers) :-
        N is NoPlayers - 1,
        calculateFinalScores(N),
        calculateSinglePlayerFinalScore(NoPlayers).


    finishingGame(NoPlayers) :-
        format('The Game is Over. We present to you the final score board:~n~n'),
        calculateFinalScores(NoPlayers),
        printResults(_).

    printOrdered([], _ ) :- !, true.

    printOrdered([ (PlayerId, Score)| OrderedResultsList], Poistion) :-
        format('~a. Player # ~a with a score of ~a.~n', [Poistion, PlayerId, Score]),
        N is Poistion + 1,
        printOrdered(OrderedResultsList, N).


    printResults(_) :-
        findall((PlayerId, Score),
            player(PlayerId, Score),
            ResultsList
        ),
        sort(2, @>=, ResultsList, OrderedResultsList),
        printOrdered(OrderedResultsList, 1).



%