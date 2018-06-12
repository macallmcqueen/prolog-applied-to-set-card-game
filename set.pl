shape(S) :- member(S, [oval, diamond, squiggle]).
num(N) :- member(N, [one, two, three]).
fill(F) :- member(F, [filled, clear, shaded]).
color(C) :- member(C, [red, green, purple]).



deck(Cs) :-
    findall(card(N, F, C, S), 
      (num(N), fill(F), color(C), shape(S)), Cs).

%% Three cards whose numbers are all different.
allDiffNums(card(A,_,_,_), card(B,_,_,_), card(C,_,_,_)) :-
	\=(A,B),
	\=(B,C),
	\=(C,A).

%% Three cards whose numbers are all the same.
allSameNums(card(A,_,_,_), card(B,_,_,_), card(C,_,_,_)) :-
	=(A,B),
	=(B,C),
	=(C,A).

%% Three cards whose fills are all different.
allDiffFills(card(_,A,_,_), card(_,B,_,_), card(_,C,_,_)) :-
	\=(A,B),
	\=(B,C),
	\=(C,A).

%% Three cards whose fills are all the same.
allSameFills(card(_,A,_,_), card(_,B,_,_), card(_,C,_,_)) :-
	=(A,B),
	=(B,C),
	=(C,A).

%% Three cards whose colors are all different.
allDiffColors(card(_,_,A,_), card(_,_,B,_), card(_,_,C,_)) :-
	\=(A,B),
	\=(B,C),
	\=(C,A).

%% Three cards whose colors are all the same.
allSameColors(card(_,_,A,_), card(_,_,B,_), card(_,_,C,_)) :-
	=(A,B),
	=(B,C),
	=(C,A).

%% Three cards whose shapes are all different.
allDiffShapes(card(_,_,_,A), card(_,_,_,B), card(_,_,_,C)) :-
	\=(A,B),
	\=(B,C),
	\=(C,A).

%% Three cards whose shapes are all the same.
allSameShapes(card(_,_,_,A), card(_,_,_,B), card(_,_,_,C)) :-
	=(A,B),
	=(B,C),
	=(C,A).

%% Three cards whose numbers are either all the same or all different.
sameOrDiffNums(A,B,C) :- allDiffNums(A,B,C).
sameOrDiffNums(A,B,C) :- allSameNums(A,B,C).

%% Three cards whose fills are either all the same or all different.
sameOrDiffFills(A,B,C) :- allDiffFills(A,B,C).
sameOrDiffFills(A,B,C) :- allSameFills(A,B,C).

%% Three cards whose colors are either all the same or all different.
sameOrDiffColors(A,B,C) :- allDiffColors(A,B,C).
sameOrDiffColors(A,B,C) :- allSameColors(A,B,C).

%% Three cards whose shapes are either all the same or all different.
sameOrDiffShapes(A,B,C) :- allDiffShapes(A,B,C).
sameOrDiffShapes(A,B,C) :- allSameShapes(A,B,C).

%% A Set is a set of three cards in which each of the four features 
%% (number, fill, color, and shape) is either all the same or 
%% all different, in any combination. 
set(A,B,C) :-
  deck(Cs), 
  member(A, Cs),
  member(B, Cs),
  member(C, Cs),
	\=(A,B),
	\=(B,C),
	\=(C,A),
        sameOrDiffNums(A,B,C),
	sameOrDiffFills(A,B,C),
	sameOrDiffColors(A,B,C),
	sameOrDiffShapes(A,B,C).
