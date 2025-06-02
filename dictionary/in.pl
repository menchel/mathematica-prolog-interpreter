father(john, bob).
father(bob, alice).
listing(a,[a,b,c]).
mother(a,b(c,d)).
grandfather(john,shlomo).
grandfather(X, Y) :- father(X, Z), father(Z, Y).
?- grandfather(john, Y).
?- grandfather(david,X).
?- +grandfather(david,X).
?- father(john, bob).
?- father(X, Y).
?- mother(a, b(c,d)).
?- mother(a, b(d,d)).
?- mother(a, Y).
?- mother(a, b(Y,d)).
?- listing(a,[a,b,c]).
?- listing(a,[a,X,c]).
?- listing(a,Y).
?- listing(a,[a,b,d]).
?- listing(a,[a,b,d]).
