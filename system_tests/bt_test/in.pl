member(X,[X|Xs]).
member(X,[Y|Ys]):-member(X, Ys).

tree(a,[b,c]).
tree(b,[d,e]).
tree(c,[k]).
tree(d,[r,t]).
tree(e,[]).
tree(k,[]).
tree(t,[]).
tree(r,[]).

parent(X,Y):-tree(X,L),member(Y,L).

path(X,X).
path(X,Y):-tree(X,L),member(Z,L),path(Z,Y).

?-parent(a,b).
?-parent(a,d).
?-+parent(k,a).
?-path(b,k).
?-path(a,X).
?-path(b,X).
?-path(a,c).
?-+path(a,c).
