first(a,b).
second(a(b),c).
third([a,b,c],d).
third([a,d,c],d).
fourth([_|[c,d]],e).
?-first(a,b).
?-first(a,c).
?-first(a,X).
?-+first(a,c).
?-second(a(b),c).
?-second(X,c).
?-second(a(X),c).
?-+second(a(b),c).
?-third([a,b,c],d).
?-third([a,X,c],d).
?-third([a,m,c],d).
?-+third([a,m,c],d).
?-third(X,d).
?-third([a|X],d).
?-fourth([a,c,d],e).
