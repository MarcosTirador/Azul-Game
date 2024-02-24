
% :- [tools].
:- dynamic strategy/2, simulation/1, snapshot/1.

simulation(0).
depth(1). %Use 1 for more velocity in the execution, and 2 for more accuracy in the strategy
snapshot([]).

strategy(1, minmax).
strategy(2, random).
strategy(3, minmax).
strategy(4, random).

printMove(Move, CurrentPlayer) :-
   Move = (Color, Fact, Amount, WhereTo),
   format('The player #~a choose to make the following move:~n',[CurrentPlayer]),
   format('~a tile(s) of color ~a from ',[Amount, Color]),
   if_else(
      Fact == 10,
         format('the center of the table '),
      true,
         format('the factory #~a ', Fact)
   ),
   format('and place it(them) '),
   if_else(
      WhereTo == 7,
         format('directly in the Floor Line.~n'),
      true,
         format('in the pattern line #~a.~n',WhereTo)

   ),
   true.


doMove(CurrentPlayer, Move) :-
   Move = (Color, IdFac, Amount, IdRow),
   removeTilesFromTable(Color, IdFac, Special),
   addTilesToPreparationBoard(CurrentPlayer, Color, Amount, IdRow, Special),
   true.


makeMove(CurrentPlayer, AvailableMoves) :-
   strategy(CurrentPlayer, Strategy),
   calculateMove(CurrentPlayer, AvailableMoves, Strategy, Move),
   printMove(Move, CurrentPlayer),
   doMove(CurrentPlayer, Move),
   !.

calculateMove(CurrentPlayer, AvailableMoves, random, Move) :-
   
   filterValidPlayerMove(CurrentPlayer, AvailableMoves, ValidPlayerMoves, ValidPlayerBadMoves),
   length(ValidPlayerMoves, NoGoodMoves),
   length(ValidPlayerBadMoves, C),
   (
      %if
      NoGoodMoves > 0,
         random_select(Move,ValidPlayerMoves,_)
      %else if
      ; NoGoodMoves == 0,
         C > 0,
         random_select(Move,ValidPlayerBadMoves,_)
      %else
      ; NoGoodMoves == 0,
         C == 0,
         true
         %Should never enter here if we control that the available moves is not an empty set.
      
   ).

calculateMove(CurrentPlayer, AvailableMoves, minmax, Move) :-
   filterValidPlayerMove(CurrentPlayer, AvailableMoves, ValidPlayerMoves, ValidPlayerBadMoves),
   
   %From the waste moves, take the one with the least tiles
   findall(FF, 
      (
         var(FF),
         member(X, ValidPlayerBadMoves),
         X = (_,_,Amount, _),
         findall(Y,
            (
               member(Y, ValidPlayerBadMoves),
               Y = (_,_,AmountY, _),
               Amount =< AmountY
            ),
            Beats
         ),
         length(Beats, BLength),
         length(ValidPlayerBadMoves, AllLength),
         BLength == AllLength,
         FF = X
      ),
      [First | _]
      ),
   append(ValidPlayerMoves, [First], Moves),


   length(AvailableMoves, NoAM),
   depth(Depth),
   simulation(Simulation_current_depth),
   if_else(
      (NoAM == 1, Simulation_current_depth > 0),  %Terminate the simulation if the next move is the last of the round
         (
            AvailableMoves = [Move | _],
            doMove(CurrentPlayer, Move),
            false
         ),
      else,
         (
            if_else(
               (Depth == Simulation_current_depth), %Also terminate if the tree reach the limit depth
                  (
                     false
                  ),
               else,
               (
                  if_else(
                     checkIfInSnapshot(CurrentPlayer, Moves, Move),
                        end,
                     else,
                        (
                           tryAllBranches(CurrentPlayer, Moves, Move), %Now try all the possible moves(the branches of the subtree)
                           takeSnapShot(CurrentPlayer, Moves, Move)
                        )
                  )
               )
            )
         )
      ),
   true.

checkIfInSnapshot(CurrentPlayer, Moves, Move) :-
   preparationBoard(CurrentPlayer, R1,R2,R3,R4,R5, Fl),
   wall(CurrentPlayer, WallTiles),
   sort(WallTiles, SortedWallTiles),
   sort(Moves, SortedMoves),

   snapshot(Snap),
   X = ((CurrentPlayer, R1,R2,R3,R4,R5,Fl,SortedWallTiles,SortedMoves), Move),
   member(X,Snap),
   end.


takeSnapShot(CurrentPlayer, Moves, Move) :-
   preparationBoard(CurrentPlayer, R1,R2,R3,R4,R5, Fl),
   wall(CurrentPlayer, WallTiles),
   sort(WallTiles, SortedWallTiles),
   sort(Moves, SortedMoves),
   
   snapshot(SnapList),
   X = ((CurrentPlayer, R1,R2,R3,R4,R5,Fl,SortedWallTiles,SortedMoves), Move),
   append(SnapList, [X], NewSnapList),
   update(snapshot, [SnapList], [NewSnapList]),
   end.


%Min Max Strategy related

   tryAllBranches(CurrentPlayer, Moves, Move) :-
      
      saveAllInfo(GameStatus),
      
      increment(simulation(_),X,_), %increments the depth of the simulation.
      if_else(X == 0, makePlayersMinMax, else, end), %Replace the players for fakes AIs
      
      findall((Try_Move, Quality), 
         (
            member(Try_Move, Moves), %for each move
            try(noPrintCall(proofBranch, [CurrentPlayer, Try_Move])), %try with one of the moves
            calculateQuality(CurrentPlayer, Quality), %Give the move a puntuation
            restoreAllInfo(GameStatus) %restore the status, from before the move
         ),
         MeasuredMoves
      ),
      sort(2, @>=, MeasuredMoves, MeasuredMovesSorted), %Sort by the given puntuation

      decrement(simulation(_), _, Simulation_current_depth),
      if_else(Simulation_current_depth == 0, restoreStrategies(GameStatus), else, end), %restore original players
      
      MeasuredMovesSorted = [(Move , _)| _], %select the move with the highest puntuation
      true.

   %branch or subtree generated by a single move
   proofBranch(CurrentPlayer, Move) :-
      doMove(CurrentPlayer ,Move),
      noPlayers(NoPlayers),
      NextPlayer is mod(CurrentPlayer, NoPlayers) + 1,
      retractall(currentPlayer(_)),
      assert(currentPlayer(NextPlayer)),
      
      nextStep(_),
      true.


   %Give a puntuation to every status of the game
   calculateQuality(CurrentPlayer, Quality) :-

      preparationBoard(CurrentPlayer, R1, R2, R3, R4, R5, Fl),
      wall(CurrentPlayer, WallTiles),

      % %Simple Score of insert in the wall the full rows
      calculatePositiveScoreAllRows([R1,R2,R3,R4,R5], WallTiles, _, NewWall, 0, WallDirectScore),
      
      %Find the amount of each color remaining in the common board
      findall( (Color, Amount),
         (
            color(Color),
            findall( (Color,Tiles),
               (
                  factory(_, Tiles),
                  member(Tile, Tiles),
                  Color == Tile;
                  center(_, Tiles2),
                  member(Tile2, Tiles2),
                  Color == Tile2
               ),
            CFT
            ),
            count(CFT, Amount)
         ),
         Color_Rest
      ),

      

      %Find the rows that are incomplete
      findall(R,
         (
            member(R, [R1,R2,R3,R4,R5]),
            R = (Color,Amount, Row),
            not(Color = blank),
            Amount =\= Row
         ),
         Incomplete_Rows
      ),

      %Find the rows which has at least one tile and are impossible to complete in this round due to the lack of tiles of that color on the table
      findall(R,
         (
            member(R, Incomplete_Rows),
            R = (Color,Amount, Row),
            member((Color, A), Color_Rest),
            RR is Row - Amount,
            A < RR
         ),
         Imposible_To_Complete_In_Round_Rows
      ),

      %Create new rows filling the incomplete ones and discarting the complete ones
      findall(Rh,
         (
            Rh = (Colorh, AM, Rowh),
            member(R, [R1, R2, R3, R4, R5]),
            R = (Color_Old, AM_Old, Rowh),
            if_else(
               (AM_Old = Rowh; Color_Old = blank),
                  (
                     Colorh = blank,
                     AM = 0
                  ),
               else,
                  (
                     Colorh = Color_Old,
                     AM = Rowh
                  )
            )
         ),
         NR
      ),

      %With the previus created rows, calculate some possible future points by inserting corresponding tiles in the wall.
      calculatePositiveScoreAllRows(NR, NewWall, _, FutureWall, 0, WallFutureScore),
      evaluateFutureWall(FutureWall, ScoreFutureWall), %Add some penality or bonus, evaluating the convenience of the wall for the future


      count(Imposible_To_Complete_In_Round_Rows, Neg1), %We substract a point for each row impossible to complete
      count(Incomplete_Rows, Neg22), %Also we substract a point for each row that is incomplete except for the first one
      Neg2 is 0.9*(Neg22 - 1),
      Neg3 is 1.1 * Neg22, %For the future scores we add a penality, for the possibility of not fulfillment
      
      Fl = (_, AmountFl, _),
      negativeScore(AmountFl, NegativeScore), %Take into account the negative score from the Floor line

      Quality is WallDirectScore + NegativeScore - Neg1 - Neg2 - Neg3 + WallFutureScore + ScoreFutureWall,
      true.
   
   %Add some penality or bonus, evaluating the convenience of the wall for the future
   evaluateFutureWall(FutureWall, ScoreFutureWall) :-
      calculateClosingScore(FutureWall, ScoreFutureWall).

   %create the fakes IAs
   makePlayersMinMax :-
      findall(_,
         (
            strategy(PlayerID, Strategy),
            update(strategy, [PlayerID, Strategy], [PlayerID, minmax])
         ),
         _
      ),
      end.

   %Save and Restore Info
   %Methods for saving and restoring the status before each move
      restoreStrategies(X) :-
         X = [Strategies | _],
         findall(_,
               (
                  member((PlayerID, Strategy), Strategies),
                  strategy(PlayerID, OldStrat),
                  update(strategy, [PlayerID, OldStrat], [PlayerID, Strategy])
               ),
               _
            ),
         end.  

      saveAllInfo(GameStatus) :- 


         findall((PlayerID, Strategy),
            strategy(PlayerID, Strategy),
            Strategies
         ),
         bag(B),
         discardPile(DP),
         center(Special, C),
         findall((FactoryID, Tiles),
            factory(FactoryID, Tiles),
            Factories
         ),
         findall((PlayerID, Stats),
            (
               player(PlayerID, _),
               saveInfo(PlayerID, Stats)
            ),
            PlayersStats
         ),
         currentPlayer(CurrentPlayer),
         GameStatus = [
            Strategies,
            CurrentPlayer,
            B,
            DP,
            Special, C,
            Factories,
            PlayersStats
         ],
         end.


      saveInfo(CurrentPlayer, X) :-
         player(CurrentPlayer, Score),
         wall(CurrentPlayer, Wall),
         preparationBoard(CurrentPlayer, R1,R2,R3,R4,R5,Fl),
         X = [
            CurrentPlayer,
            Score,
            Wall,
            R1,R2,R3,R4,R5,Fl
         ],
         true.


      restoreAllInfo(GameStatus) :-
         GameStatus = [
            _,
            CurrentPlayer,
            B,
            DP,
            Special, C,
            Factories,
            PlayersStats
         ],
         findall(_,
            (
               member((PlayerID,Stats), PlayersStats),
               restoreInfo(PlayerID, Stats)
            ),
            _
         ),
         retract(bag(_)),
         retract(discardPile(_)),
         retract(center(_, _)),
         retractall(factory(_,_)),


         update(currentPlayer, [_], [CurrentPlayer]),
         assert(bag(B)),
         assert(discardPile(DP)),
         assert(center(Special, C)),

         findall((FactoryID, Tiles),
               (
                  member((FactoryID,Tiles), Factories),
                  assert(factory(FactoryID, Tiles))
               ),
               _
            ),
         end.

      restoreInfo(CurrentPlayer, X) :-
         

         retract(player(CurrentPlayer, _)),
         retract(wall(CurrentPlayer, _)),
         retract(preparationBoard(CurrentPlayer, _,_,_,_,_, _)),
         X = [
            CurrentPlayer,
            Score,
            Wall,
            R1,R2,R3,R4,R5,Fl
         ],
         
         assert(player(CurrentPlayer, Score)),
         assert(wall(CurrentPlayer, Wall)),
         assert(preparationBoard(CurrentPlayer, R1, R2, R3, R4, R5, Fl)),
         true.

   %
%





