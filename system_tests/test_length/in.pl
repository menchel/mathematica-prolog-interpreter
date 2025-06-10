length([],zero).
length([_|X],s(Y)):-length(X,Y).
?-length([],X).
?-length([a,b,c],X).
