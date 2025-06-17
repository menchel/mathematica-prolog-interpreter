member(X,[X|Xs]).
member(X,[Y|Ys]):-member(X, Ys).

tree(a,[b,c]).
tree(b,[d,e]).
tree(c,[k]).
tree(d,[]).
tree(e,[]).
tree(k,[]).

path(X,X).
path(X,Y):-tree(X,L),member(Z,L),path(Z,Y).

?-path(b,k).
