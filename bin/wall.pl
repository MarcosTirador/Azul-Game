
color(red).
color(yellow).
color(blue).
color(black).
color(white).

column(blue, 1).
column(white, 2).
column(black, 3).
column(red, 4).
column(yellow, 5).

next(blue, yellow).
next(yellow, red).
next(red, black).
next(black, white).
next(white, blue).

previous(X, Y) :- next(Y, X).

colorMissingInRowInWall(CurrentPlayer, Row, Ou) :-
    wall(CurrentPlayer, List),
    findall(Color, (color(Color), not(member((Row,Color), List))), Ou).


%Calculate Score In the Row

    goLeft(Row, PreviousColor, _, InitialScore, EndScore) :-
        column(PreviousColor, Diff),
        Column is mod(Row + 5 - Diff, 5) + 1,
        Column == 5,
        !,
        EndScore is InitialScore.
    goLeft(Row, PreviousColor, WallTiles, InitialScore, EndScore) :-
        not(member((Row, PreviousColor), WallTiles)),
        !,
        EndScore is InitialScore,
        true.
    goLeft(Row, PreviousColor, WallTiles, InitialScore, EndScore) :-
        member((Row, PreviousColor), WallTiles),
        !,
        EndScore_1 is InitialScore + 1,
        previous(PreviousColor, PrevPrev),
        goLeft(Row, PrevPrev, WallTiles, EndScore_1, EndScore).



    goRight(Row, NextColor, _, InitialScore, EndScore) :-
        column(NextColor, Diff),
        Column is mod(Row + 5 - Diff, 5) + 1,
        Column == 1,
        !,
        EndScore is InitialScore.
    goRight(Row, NextColor, WallTiles, InitialScore, EndScore) :-
        not(member((Row, NextColor), WallTiles)),
        EndScore is InitialScore,
        !.
    goRight(Row, NextColor, WallTiles, InitialScore, EndScore) :-
        member((Row, NextColor), WallTiles),
        !,
        EndScore_1 is InitialScore + 1,
        next(NextColor, NextNext),
        goRight(Row, NextNext, WallTiles, EndScore_1, EndScore).


    calculateSumInRow(Row, Color, WallTiles,RowSum) :-
        next(Color,NextColor),
        previous(Color, PreviousColor),
        goRight(Row, NextColor, WallTiles, 0, ScoreToRight),
        goLeft(Row, PreviousColor, WallTiles, 0, ScoreToLeft),
        RowSum is ScoreToLeft + ScoreToRight + 1.
%

%Calculate Score In the Column
    goDown(Row, _, _, InitialScore, EndScore) :-
        Row == 1,
        !,
        EndScore is InitialScore.
    goDown(Row, PreviousColor, WallTiles, InitialScore, EndScore) :-
        not(member((Row, PreviousColor), WallTiles)),
        EndScore is InitialScore,
        !.
    goDown(Row, PreviousColor, WallTiles, InitialScore, EndScore) :-
        member((Row, PreviousColor), WallTiles),
        !,
        EndScore_1 is InitialScore + 1,
        RowDown is mod(Row + 1, 5),
        previous(PreviousColor, PrevPrev),
        goDown(RowDown, PrevPrev, WallTiles, EndScore_1, EndScore).



    goUp(Row, _, _, InitialScore, EndScore) :-
        Row == 5,
        !,
        EndScore is InitialScore.
    goUp(Row, NextColor, WallTiles, InitialScore, EndScore) :-
        not(member((Row, NextColor), WallTiles)),
        EndScore is InitialScore,
        !.
    goUp(Row, NextColor, WallTiles, InitialScore, EndScore) :-
        member((Row, NextColor), WallTiles),
        !,
        EndScore_1 is InitialScore + 1,
        next(NextColor, NextNext),
        RowUp is mod(Row + 4, 5),
        goUp(RowUp, NextNext, WallTiles, EndScore_1, EndScore).


    calculateSumInColumn(Row, Color, WallTiles,ColumnSum) :-
        next(Color,NextColor),
        RowUp is mod(Row + 3, 5) + 1,
        previous(Color, PreviousColor),
        RowDown is mod(Row, 5) + 1,
        goUp(RowUp, NextColor, WallTiles, 0, ScoreUp),
        goDown(RowDown, PreviousColor, WallTiles, 0, ScoreDown),
        ColumnSum is ScoreDown + ScoreUp + 1.
%



calculateScoreByInsertingInWall(Row, Color, WallTiles, ScoreW) :-
       calculateSumInRow(Row, Color, WallTiles,RowSum),
       calculateSumInColumn(Row, Color, WallTiles,ColumnSum),

       if_else(
            (ColumnSum == 1 ; RowSum == 1),
                (ScoreW is ColumnSum + RowSum - 1),
            true,
                (ScoreW is ColumnSum + RowSum)
       ),
       true.

checkIfFullRow(Wall, Ended) :-
    findall((Row,Amount),
        (
            member(Row,[1,2,3,4,5]),
            findall(Row,
                member((Row,_), Wall),
                LR
            ),
            length(LR,Amount),
            Amount == 5
        ),
        FullRows
    ),
    length(FullRows , L),
    if_else(
            L > 0,
            Ended is 1,
            L =< 0,
            Ended is 0
        ),
    !.


% Calculate the scores of closing

    calculateSameColorBonuses(WallTiles, SameColorBonusScore) :-
        findall((Color,Amount),
            (
                color(Color),
                findall(Row,
                    member((Row,Color), WallTiles),
                    LR
                ),
                length(LR,Amount),
                Amount == 5
            ),
            FullColor
        ),
        length(FullColor, CFullC),
        SameColorBonusScore is 10 * CFullC.

    checkIfSameColumn(Row, Column, Color, WallTiles, Amount) :-
        column(Color, Clm),
        Column2 is mod(Row + 5 - Clm, 5) + 1,
        Column == Column2,
        end.


    calculateColumnBonuses(WallTiles, ColumnBonusScore) :-
        
        findall((Column,Amount),
            (
                member(Column,[1,2,3,4,5]),
                findall(Row,
                    (
                        member((Row,Color), WallTiles),
                        checkIfSameColumn(Row, Column, Color, WallTiles, Amount),
                        true
                    ),
                    LR
                ),
                length(LR,Amount),
                Amount == 5
            ),
            FullColumns
        ),
        length(FullColumns, CFullC),
        ColumnBonusScore is 5 * CFullC.

    calculateRowBonuses(WallTiles, RowBonusScore) :-
        findall((Row,Amount),
            (
                member(Row,[1,2,3,4,5]),
                findall(Row,
                    member((Row,_), WallTiles),
                    LR
                ),
                length(LR,Amount),
                Amount == 5
            ),
            FullRows
        ),
        length(FullRows, CFullR),
        RowBonusScore is 2 * CFullR.

    %Calculate the additional scores at the end of the game.
    calculateClosingScore(WallTiles, Score) :-
        calculateRowBonuses(WallTiles, RowBonusScore),
        calculateColumnBonuses(WallTiles, ColumnBonusScore),
        calculateSameColorBonuses(WallTiles, SameColorBonusScore),
        Score is RowBonusScore + ColumnBonusScore + SameColorBonusScore.
     

%

printRowWall(WallTiles, R, PR) :-
    findall(Rep,
        (
            member(C, [1,2,3,4,5]),
            color(Color),
            column(Color,Diff),
            Column is mod(R - Diff + 5, 5) + 1,
            Column == C,
            if_else(
                member((R,Color), WallTiles),
                (toShowColor(Color,Rep)),
                true,
                (Rep = '_')
            )
        ),
        PR
    ).

makeGuideWall(GuideWall) :-
    findall((Row, Color),
        (
            member(Row, [1,2,3,4,5]),
            color(Color)
        ),
        GuideWall
    ),
    true.


printWall(WallTIles) :-
    findall(_,
        (
            member(R,[1,2,3,4,5]),
            printRowWall(WallTIles, R, PR),
            format('~a ~a ~a ~a ~a ~n', PR)
        ),
        _
    ).


