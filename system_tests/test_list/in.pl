member(X,[X|Xs]).
member(X,[Y|Ys]):-member(X, Ys).
prefix([], _).
prefix([X|Xs], [X|Ys]) :- prefix(Xs, Ys).
del(X, [X|Xs], Xs).
del(X, [Y|Ys], [Y|Zs]) :- del(X, Ys, Zs).
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
?-member(X,[a,b,c]).
?-member(a(b),[c,d,a(b)]).
?-member(a(d),[c,d,a(b)]).
?-+member(a(b),[c,d,a(b)]).
?-prefix([],[a,b,c,d]).
?-prefix([a,b,c],[a,b,c,d]).
?-prefix([a,b,c,d,e],[a,b,c,d]).
?-del(a,[c,a,b],X).
?-del(a(d),[c,a(b),a(d)],X).
?-del(a,[a],X).
?-append([a,b],[c,d,e],X).
?-append([a,b],[c,d,e],[a,b,d,d,e]).
?-append(Y,[c,d,e],[a,b,c,d,e]).
?-append([a,b],Z,[a,b,c,d,e]).
