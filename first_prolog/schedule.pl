divide([],[],[]).
divide([A],[A],[]).
divide([A|B],[A|C1],C2) :- divide(B,C2,C1).
merge([],[],[]).
merge([X],[],[X]).
merge([A1|A],[B1|B],[A1|C]) :- merge(A,[B1|B],C),A1 =< B1 .
merge(A,B,C) :- merge(B,A,C).
mergesort([],[]).
mergesort([D],[D]).
mergesort(A,B) :- divide(A,A1,A2) , mergesort(A1,B1) , mergesort(A2,B2) , merge(B1,B2,B).