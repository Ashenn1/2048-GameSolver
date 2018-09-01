play:-
  write('================'),
  nl,    
  write('welcome to 2048'),
  nl,
  write('================'),
  nl,
  Board=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
  putValue(Board,L),
  putValue(L,NewBoard),
  nl, write('Rand play : '), nl,
  show(NewBoard),
  play( [computerPlayer , play , NewBoard] ).
  
play( [computerPlayer , play , Board] ) :-
    nl, write('Computer play : '), nl,
    bestMove([computerPlayer, play, Board], [NextPlayer, State, BestSuccBoard]),
    write('BestSuccBoard:'),nl,show(BestSuccBoard),
    (
      State = win, !,                                 
      nl, write('End of game : '),
      write(' Computer wins !'), nl
      ;
      State = lose, !,                                
      nl, write('Game Over'), nl
      ; State=play,!,
	  play( [ NextPlayer , play, BestSuccBoard] )
	  ; 1=1
	  %(BestSuccBoard=Board ,!,play([computerPlayer , play , BestSuccBoard]); play( [ randPlayer, play, BestSuccBoard] ))
    ).
	
play( [randPlayer, play, Board] ) :- 
     nl, write('Rand play : '), nl,
    ( randMove([randPlayer, play, Board], [NextPlayer, State, NextBoard]), !,
      write('After adding random number:'),nl,show(NextBoard),
        (   State = lose,!, nl,
			write('Game Over'), nl
			;
			play([ NextPlayer, play, NextBoard] ) 
        )
	    ;
		1=1
    ).

	
randMove([X1, play, Board], [X2, play, NextBoard]) :-
    nextPlayer(X1, X2),
    putValue(Board, NextBoard).

nextPlayer(computerPlayer, randPlayer).
nextPlayer(randPlayer, computerPlayer).
  

  
bestMove(Pos, [Player,State,NextBoard]) :- 
    minimax(Pos, [Player,State,NextBoard], _),
	write('showing next best move : '),nl,
	show(NextBoard).

	

minimax(Pos, BestNextPos, Val) :-                     % Pos has successors
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    best(NextPosList, BestNextPos, Val), !.

minimax(Pos, _, Val) :-                     % Pos has no successors
    utility(Pos, Val),
	write('in the utility'),nl.

best([Pos], Pos, Val) :-
    minimax(Pos, _, Val),write('in the best of [pos]'),nl,!.

best([Pos1 | PosList], BestPos, BestVal) :-
    minimax(Pos1, _, Val1), 
	best(PosList, Pos2, Val2),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal),
	write('after the better of'),nl.
	

betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    min_to_move(Pos0),                         % MIN to move in Pos0
    Val0 > Val1, !                             % MAX prefers the greater value
    ;
    max_to_move(Pos0),                         % MAX to move in Pos0
    %Val0 < Val1,
	write('in the better of predicate'),nl,
	!.                            % MIN prefers the lesser value

betterOf(_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0

	

move([X1, play, Board],[X2, win, NextBoard]) :-
	nextPlayer(X1, X2),
	move_aux([X1,play,Board], [X1,play,NextBoard]),
	%\+same(Board,NextBoard),
    win(128,NextBoard), !.
 
move([X1, play, Board],[X2, lose, NextBoard]) :-
	nextPlayer(X1, X2),
	move_aux([X1,play,Board], [X1,play,NextBoard]),
	%\+same(Board,NextBoard),
    lose(NextBoard), !.
 
 
move([X1, play, Board], [X2, play, NextBoard]) :-
	nextPlayer(X1, X2),
	move_aux([X1,play,Board], [X1,play,NextBoard]),
	%\+same(Board,NextBoard),
	show(NextBoard).
	


min_to_move([randPlayer, _, _]).

max_to_move([computerPlayer, _, _]).


win_aux(X,[Player,State,NextBoard]):-
win(X,NextBoard).



win(X, [X|Xs]).  %is true if X is in the list -> used to check if 2048 exists
win(X, [Y|Ys]) :-
    win(X, Ys),!.
	
lose([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]):- % lma ykon mfesh 2 cells gnb b3d b nfs el value and kol el cells not equal zero
	X1\=0,X2\=0,X3\=0,X4\=0,X5\=0,X6\=0,X7\=0,X8\=0,
	X9\=0,X10\=0,X11\=0,X12\=0,X13\=0,X14\=0,X15\=0,X16\=0,
	
	X1\=X2,X1\=X5,X2\=X3,X2\=X6,X3\=X7,X3\=X4,X4\=X8,
	X5\=X6,X5\=X9,X6\=X7,X6\=X10,X7\=X8,X7\=X11,X8\=X12,
	X9\=X10,X9\=X13,X10\=X11,X10\=X14,X11\=X12,
	X11\=X15,X12\=X16,X13\=X14,X14\=X15,X15\=X16,!.
	
%show =======================================================================================================================
show([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]):-
  write('    ---------------------'), nl,
  write('   '),write('|'), show2(X1),
  write('|'), show2(X2),
  write('|'), show2(X3),
  write('|'), show2(X4),write('|'), nl, 
  write('    ---------------------'), nl,
  write('   '),write('|'), show2(X5),
  write('|'), show2(X6),
  write('|'), show2(X7),
  write('|'), show2(X8),write('|'), nl,
  write('    ---------------------'), nl,
  write('   '),write('|'), show2(X9),
  write('|'), show2(X10),
  write('|'), show2(X11),
  write('|'), show2(X12),write('|'), nl,
  write('    ---------------------'), nl,
  write('   '),write('|'), show2(X13),
  write('|'), show2(X14),
  write('|'), show2(X15),
  write('|'), show2(X16),write('|'), nl,
  write('    ---------------------'), nl.

show2(0):-
  write('    '),!.

show2(X):-
	digits(X,Y),
	(Y=1 ,!, write('   ');1=1),(Y=2 ,!, write('  '); 1=1),(Y=3 ,!, write(' '); 1=1),write(X),!.

digits(X,1):- % counts number of digits of an integer
	10>X,X>0,!.
	digits(X,Y):- A=X/10,digits(A,B),Y is B+1 ,!.
	
%random ======================================================================================================================= 

twoOrFour(N) :-
	P is random(10),
	P < 9 -> N is 2 ; N is 4.	% 90% of the time it generates 2 , other times it generates 4.
		
randIndex([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],Indexx):- % gets a random index of a zero valued place.
	Indexx is random(15),
    nth0(Indexx,[X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],V,_),
	V is 0 -> 1=1 ; randIndex([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],Indexx).

putValue([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],L):- % puts 2 or 4 in a zero valued place and return a new list
   randIndex([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],Index),
   twoOrFour(Val),
   nth0(Index, [X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16] , _, R),
   nth0(Index, L, Val, R).
   
% moves =======================================================================================================================

move_aux([XX,X,[X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]],[XX,X,[Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12, Y13, Y14, Y15, Y16]]):-
	XX=randPlayer,
	putValue([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],[Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12, Y13, Y14, Y15, Y16]),!.

move_aux([XX,X,[X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]], [XX,X,[Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12, Y13, Y14, Y15, Y16]]):-
	moveUp([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],[Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12, Y13, Y14, Y15, Y16]),
	\+ same([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],[Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12, Y13, Y14, Y15, Y16]).

move_aux([XX,X,[X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]], [XX,X,[Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12, Y13, Y14, Y15, Y16]]):-
	moveDown([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],[Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12, Y13, Y14, Y15, Y16]),
	\+ same([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],[Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12, Y13, Y14, Y15, Y16]).

move_aux([XX,X,[X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]], [XX,X,[Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12, Y13, Y14, Y15, Y16]]):-
	moveRight([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],[Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12, Y13, Y14, Y15, Y16]),
	\+ same([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],[Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12, Y13, Y14, Y15, Y16]).
	
move_aux([XX,X,[X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]], [XX,X,[Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12, Y13, Y14, Y15, Y16]]):-
	moveLeft([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],[Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12, Y13, Y14, Y15, Y16]),
	\+ same([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],[Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12, Y13, Y14, Y15, Y16]).

	
	
moveUp([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],[XX1, XX2, XX3, XX4, XX5, XX6, XX7, XX8, XX9, XX10, XX11, XX12, XX13, XX14, XX15, XX16]):-
	%show([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]),
	liftAll([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],[NX1, NX2, NX3, NX4, NX5, NX6, NX7, NX8, NX9, NX10, NX11, NX12, NX13, NX14, NX15, NX16]),
	mergeUp([NX1, NX2, NX3, NX4, NX5, NX6, NX7, NX8, NX9, NX10, NX11, NX12, NX13, NX14, NX15, NX16],[XX1, XX2, XX3, XX4, XX5, XX6, XX7, XX8, XX9, XX10, XX11, XX12, XX13, XX14, XX15, XX16]),!.
	%show([XX1, XX2, XX3, XX4, XX5, XX6, XX7, XX8, XX9, XX10, XX11, XX12, XX13, XX14, XX15, XX16]).

moveDown([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],[XX1, XX2, XX3, XX4, XX5, XX6, XX7, XX8, XX9, XX10, XX11, XX12, XX13, XX14, XX15, XX16]):-
	%show([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]),
	liftAll([X16, X15, X14, X13, X12, X11, X10, X9, X8, X7, X6, X5, X4, X3, X2, X1],[NX16, NX15, NX14, NX13, NX12, NX11, NX10, NX9, NX8, NX7, NX6, NX5, NX4, NX3, NX2, NX1]),
	mergeUp([NX16, NX15, NX14, NX13, NX12, NX11, NX10, NX9, NX8, NX7, NX6, NX5, NX4, NX3, NX2, NX1],[XX16, XX15, XX14, XX13, XX12, XX11, XX10, XX9, XX8, XX7, XX6, XX5, XX4, XX3, XX2, XX1]),!.
	%show([XX1, XX2, XX3, XX4, XX5, XX6, XX7, XX8, XX9, XX10, XX11, XX12, XX13, XX14, XX15, XX16]).
	
moveRight([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],[XX1, XX2, XX3, XX4, XX5, XX6, XX7, XX8, XX9, XX10, XX11, XX12, XX13, XX14, XX15, XX16]):-
	%show([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]),
	liftAll([X4, X8, X12, X16, X3, X7, X11, X15, X2, X6, X10, X14, X1, X5, X9, X13],[NX4, NX8, NX12, NX16, NX3, NX7, NX11, NX15, NX2, NX6, NX10, NX14, NX1, NX5, NX9, NX13]),
	mergeUp([NX4, NX8, NX12, NX16, NX3, NX7, NX11, NX15, NX2, NX6, NX10, NX14, NX1, NX5, NX9, NX13],[XX4, XX8, XX12, XX16, XX3, XX7, XX11, XX15, XX2, XX6, XX10, XX14, XX1, XX5, XX9, XX13]),!.
	%show([XX1, XX2, XX3, XX4, XX5, XX6, XX7, XX8, XX9, XX10, XX11, XX12, XX13, XX14, XX15, XX16]).

moveLeft([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],[XX1, XX2, XX3, XX4, XX5, XX6, XX7, XX8, XX9, XX10, XX11, XX12, XX13, XX14, XX15, XX16]):-
	%show([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]),
	liftAll([X13, X9, X5, X1, X14, X10, X6, X2, X15, X11, X7, X3, X16, X12, X8, X4],[NX13, NX9, NX5, NX1, NX14, NX10, NX6, NX2, NX15, NX11, NX7, NX3, NX16, NX12, NX8, NX4]),
	mergeUp([NX13, NX9, NX5, NX1, NX14, NX10, NX6, NX2, NX15, NX11, NX7, NX3, NX16, NX12, NX8, NX4],[XX13, XX9, XX5, XX1, XX14, XX10, XX6, XX2, XX15, XX11, XX7, XX3, XX16, XX12, XX8, XX4]),!.
	%show([XX1, XX2, XX3, XX4, XX5, XX6, XX7, XX8, XX9, XX10, XX11, XX12, XX13, XX14, XX15, XX16]).

%mergeUp([2,4,32,8,8,16,4,0,2,4,2,4,2,0,2,0],[YX1, YX2, YX3, YX4, YX5, YX6, YX7, YX8, YX9, YX10, YX11, YX12, YX13, YX14, YX15, YX16]).
mergeUp([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],[NX1, NX2, NX3, NX4, NX5, NX6, NX7, NX8, NX9, NX10, NX11, NX12, NX13, NX14, NX15, NX16]):-
	mergeCol([X1, X5, X9, X13],[NX1, NX5, NX9, NX13]),
	mergeCol([X2, X6, X10, X14],[NX2, NX6, NX10, NX14]),
	mergeCol([X3, X7, X11, X15],[NX3, NX7, NX11, NX15]),
	mergeCol([X4, X8, X12, X16],[NX4, NX8, NX12, NX16]),!.
	
mergeCol([X, X, Y, Y],[NX1, NX2, 0, 0]):- NX1 is X*2,NX2 is Y*2,!.
	
mergeCol([X, X, X1, X2],[NX1, X1, X2, 0]):- NX1 is X*2,!.
	
mergeCol([X1, X2, X ,X],[X1, X2, NX1, 0]):- NX1 is X*2,!.

mergeCol([X1, X, X, X2],[X1, NX1, X2, 0]):- NX1 is X*2,!.

mergeCol([X1, X2, X3, X4],[X1, X2, X3, X4]).
	
liftAll([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],[NX1, NX2, NX3, NX4, NX5, NX6, NX7, NX8, NX9, NX10, NX11, NX12, NX13, NX14, NX15, NX16]):-
	liftCol([X1, X5, X9, X13],[NX1, NX5, NX9, NX13]),
	liftCol([X2, X6, X10, X14],[NX2, NX6, NX10, NX14]),
	liftCol([X3, X7, X11, X15],[NX3, NX7, NX11, NX15]),
	liftCol([X4, X8, X12, X16],[NX4, NX8, NX12, NX16]),!.

%liftAll([0,0,0,0,4,2,0,0,16,0,0,0,4,8,0,2],[X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]).
%liftCol([0, X2, X3, X4],[X2, X3, X4, 0]).
%liftCol([X1, 0, X3, X4],[X1, X3, X4, 0]).
%liftCol([X1, X2, 0, X4],[X1, X2, X3, 0]).
%liftCol([X1, X2, X3, 0],[X1, X2, X3, 0]).
%liftCol([0, 0, X3, X4],[X3, X4, 0, 0]).
%liftCol([X1, 0, 0, X4],[X1, X4, 0, 0]).
%liftCol([X1, X2, 0, 0],[X1, X2, 0, 0]).
%liftCol([X1, 0, 0, 0],[X1, 0, 0, 0]).
%liftCol([0, 0, 0, X4],[X4, 0, 0, 0]).
%liftCol([0, 0, 0, 0],[0, 0, 0, 0]).
%liftCol([, , , ],[, , , ]).

liftCol([X1, X5, X9, X13],[NX1, NX5, NX9, NX13]):-
	liftCell1([X1, X5, X9, X13],[YX1, YX5, YX9, YX13]),
	liftCell2([YX1, YX5, YX9, YX13],[XX1, XX5, XX9, XX13]),
	liftCell3([XX1, XX5, XX9, XX13],[NX1, NX5, NX9, NX13]),!.

liftCell1([X1, X5, X9, X13],[NX1, NX5, NX9, NX13]):- % a cell in row 1 to be updated wa2t el move
	(X1=0 ,!,
			(X5\=0 ,!,
					NX1 is X5, NX5 is 0, NX9 is X9, NX13 is X13
					;
				    (X9\=0 ,!,
						    NX1 is X9, NX9 is 0,NX5 is X5, NX13 is X13
						    ;
						    (X13\=0 ,!,
								    NX1 is X13, NX13 is 0, NX5 is X5, NX9 is X9
								    ;
									NX1 is X1, NX5 is X5, NX9 is X9, NX13 is X13
								    
							)
					)
			)%,(var(NX5),!,NX5 is X5;(var(NX9),!,NX9 is X9;(var(NX13),!,NX13 is X13;1=1)))
			;
            NX1 is X1, NX5 is X5, NX9 is X9, NX13 is X13
	).
	
liftCell2([X1, X5, X9, X13],[NX1, NX5, NX9, NX13]):- % a cell in row 2 to be updated wa2t el move
	(X5=0 ,!,
			(X9\=0 ,!,
					NX5 is X9, NX9 is 0,NX1 is X1, NX13 is X13
					;
					(X13\=0 ,!,
							NX5 is X13, NX13 is 0,NX1 is X1, NX9 is X9
							;
							NX1 is X1, NX5 is X5, NX9 is X9, NX13 is X13
					)
			)
			;
			NX1 is X1, NX5 is X5, NX9 is X9, NX13 is X13
	).

liftCell3([X1, X5, X9, X13],[NX1, NX5, NX9, NX13]):-  % a cell in row 3 to be updated wa2t el move
	(X9=0 ,!,
			(X13\=0 ,!,
					NX9 is X13, NX13 is 0,NX1 is X1, NX5 is X5
					;
					NX1 is X1, NX5 is X5, NX9 is X9, NX13 is X13
			)
			;
			NX1 is X1, NX5 is X5, NX9 is X9, NX13 is X13
	).
%utility=======================================================================================================================
count([],0). % can be used in heuristic function
count([H|T],R):-
    count(T,CT),
	(H=0 -> R is CT + 1; R is CT).

utility([_,_,[X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]],Val):-
	count([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16],C),
	A is X1*5 +X2*5 +X3*5 +X4*5 + X5*10 + X6*10+ X7*10+ X8*10+ X9*15+ X10*15+ X11*15+ X12*15+ X13*20+ X14*20+ X15*20+ X16*20,
	Val is C*10+A,!.

	
	
	
same([], []).

same([H1|R1], [H2|R2]):-
    H1 = H2,
    same(R1, R2).