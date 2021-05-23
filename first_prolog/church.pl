takeout(X,[X|T],T).
takeout(X,[Y|T],[Y|S]) :- takeout(X,T,S).
prefix([],X).
prefix([X|A],[X|B]):- prefix(A,B).
suffix(X,X).
suffix(A,[X|B]) :- suffix(A,B).
sublist(A,B) :- prefix(C,B),suffix(A,C).
append([],X,X).
append([X|A],B,[X|C]) :- append(A,B,C).
reverse([X],[X]). 
reverse([X|A],B) :- reverse(A,Ar),append(Ar,[X],B).
perm([],[]).
perm([H|A],B) :- perm(A,C),takeout(H,B,C).