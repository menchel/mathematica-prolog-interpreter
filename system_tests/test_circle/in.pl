edge(a, b).
edge(b, c).
edge(c, d).
edge(d, e).
edge(e, f).
edge(f, g).
edge(g, b).

path(X,X).
path(X,Y):- edge(X,Z),path(Z,Y).

?-path(f,g).
?-path(g,f).
?-path(a,b).
?-path(b,a).
?-+path(g,f).
