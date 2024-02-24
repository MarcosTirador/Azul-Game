:- [board, strategy].
:- dynamic started/1, noPlayers/1, noFactories/1, currentPlayer/1, gameOver/1, round/1.

started(non_started).
currentPlayer(1).

%allow us to start a new game in the same compilation
reset(_) :-
    %here
    retractall(noPlayers(_)),
    retractall(noFactories(_)),
    retractall(currentPlayer(_)),
    retractall(gameOver(_)),
    retractall(started(_)),

    %Board:
    retractall(preparationBoard(_,_,_,_,_,_,_)),
    retractall(player(_,_)),
    retractall(factory(_,_)),
    retractall(center(_,_)),
    retractall(bag(_)),
    retractall(discardPile(_)),
    retractall(wall(_,_)),

    retractall(started(_)),
    retractall(currentPlayer(_)),
    assert(started(non_started)),
    assert(currentPlayer(1)),
    
    %Strategy
    retractall(strategy(_, _)),
    assert(strategy(1, minmax)),
    assert(strategy(2, random)),
    assert(strategy(3, random)),
    assert(strategy(4, random)),

    update(simulation, [_], [0]),

    end.
    


startGame(NoPlayers) :-
    started(X),
    X == non_started,
    !,
    retractall(started(_)),
    assert(started(already_begun)),

    createPlayers(NoPlayers),
    assert(noPlayers(NoPlayers)),   

    resetBag(_),
    createDiscardPile(_),
    createCenter(_),

    rNFNP(NoPlayers, NoFactories),
    assert(noFactories(NoFactories)),    
    createFactories(NoFactories),
    pickAllFactoriesTiles(NoFactories),
    
    printStatus(_),
    assert(round(1)),
    nextStep(_), %This line can be commented so we can simulate the game one round at the time using the command nextStep(_) directly from the console.
    !.

startGame(_) :-
    started(already_begun),
    !,
    format('A game is already in course ~n', _)
    .

nextStep(_):-
    started(X),
    not(X = already_begun),
    !,
    format('The game has not begun yet ~n', _).

nextStep(_):-
    started(already_begun),
    noFactories(NoFactories), noPlayers(NoPlayers), currentPlayer(CurrentPlayer),
    
    obtainAvailableMoves(NoFactories, AvailableMoves),
    length(AvailableMoves, NoAM),
    not(NoAM = 0), %The Are still available moves...
    !,
    
    %Uncomment the next block of comments if want to see the status before each move

    % printStatus(_),
    % if_else( %Just print once in each cycle the general status.
    %     CurrentPlayer == 1,
    %         printAllPlayersBoards(NoPlayers),
    %     true,
    %         true
    % ),

    % printAllPlayersBoards(NoPlayers), %If you want to print all, in each turn, uncomment this and comment the previous if.

    makeMove(CurrentPlayer, AvailableMoves),
    % printPlayerPatternLines(CurrentPlayer),  %uncomment this if want to see the board of the player after his move

    NextPlayer is mod(CurrentPlayer, NoPlayers) + 1,
    retractall(currentPlayer(_)),
    assert(currentPlayer(NextPlayer)),
    
    nextStep(_),%This line can be commented so we can simulate the game one round at the time using the command nextStep(_) directly from the console.
    true.

nextStep(_):- % there is no other moves

    noPlayers(NoPlayers),
    printAllPlayersBoards(NoPlayers),
    format('~n ---------------------------------------------There are not moves left-----------------------------------------------------~n~n', []),
    actualizeAllScores(NoPlayers, NextPlayer),

    format('~n~n The results of the round are:~n~n', []),
    printAllPlayersBoards(NoPlayers), 
    format('~n', []),
    printPlayerStatus(_),
    format('~n', []),
    

    checkIfGameOver(NoPlayers, Over),
    if_else(
        Over >= 1,
            finishingGame(NoPlayers),
        true,
            (
                format('~n ---------------------------------------------Next Round----------------------------------------------------~n', []),
                prepareNextRound(_),
                printStatus(_),

                retractall(currentPlayer(_)),
                assert(currentPlayer(NextPlayer)),
                
                round(Round),
                Round2 is Round + 1,
                retractall(round(_)),
                assert(round(Round2)),

                nextStep(_), %Comment this line if you want to go one round at the time
                true
            )
    ).

prepareNextRound(_) :-
    noFactories(NoFactories),
    pickAllFactoriesTiles(NoFactories),
    resetCenter(_),
    true.




    






    
