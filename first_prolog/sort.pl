isort(X,Y) :- permutation(X,Y),ordered(Y). 
ordered([]).
ordered([D]).
ordered([F,S|T]) :- lesseq(F,S),ordered([S|T]). 
lesseq(F,S) :- F=<S.
permutation([],[]).
permutation([H|T], [F|R]) :- delete(F,[H|T],Z),permutation(Z,R). 
delete(H,[H|T],T).
delete(X,[H|T],[H|U]) :- delete(X,T,U).