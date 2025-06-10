father(john,bob).
father(bob,david).
grandfather(X,Y):- father(X,Z),father(Z,Y).
listing(a(b,c),[q,w,e,r]).
holdthetest(_,john).
?- father(X,bob).
?- father(X,Y).
?- father(david,bob).
?- grandfather(john,Y).
?- grandfather(john,david).
?- grandfather(david,bob).
?- listing(a(b,c),[q,w,e,r]).
?- listing(a(b,t),[q,w,e,r]).
?- listing(t(b,c),[q,w,e,r]).
?- listing(a(b,c),[q,w,t,r]).
?- listing(a(Z,c),[q,Y,e,X]).
?- listing(X,[q,w,e,r]).
?- listing(a(b,c),Y).
?- listing(a(b,c),[X|Y]).
?- holdthetest(a,john).
?- holdthetest(b,john).
?- holdthetest(a,david).
?- +holdthetest(a,david).
