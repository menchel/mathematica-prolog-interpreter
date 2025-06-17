add(zero,X,X).
add(s(X),Y,s(Z)):-add(X,Y,Z).
smaller(zero,s(_)).
smaller(s(X),s(Y)):-smaller(X,Y).
member(X,[X|Xs]).
member(X,[Y|Ys]):-member(X, Ys).
length([],zero).
length([_|X],s(Y)):-length(X,Y).

smaller_than_all(_,[]).
smaller_than_all(Root,[bt(X,_)|XS]):- smaller(Root,X), smaller_than_all(Root,XS).

bt_of_size_iterate(_,zero).
bt_of_size_iterate([bt(X,List)|XS],Curr):-add(Next,s(zero),Curr),bt_of_size(X,List,Next),bt_of_size_iterate(XS,Next).

bt_of_size(_,[],zero).
bt_of_size(Root,List,Size):-smaller(zero,Size),smaller_than_all(Root,List),bt_of_size_iterate(List,Size).

bt(Root,List):-length(List,Size).

?-smaller_than_all(s(zero),[bt(s(zero),[]),bt(s(s(zero)),[])]).
?-smaller_than_all(s(zero),[bt(s(s(zero)),[]),bt(s(s(zero)),[])]).
?-bt_of_size(s(zero),[],s(zero)).
