node(X, Val, Y, L, R).

split(nil, K, nil, nil):- !.

split(node(X, Val, Y, L, R), K, node(X, Val, Y, L, R1), S):- K >= X, split(R, K, R1, S).%right split

split(node(X, Val, Y, L, R), K, F, node(X, Val, Y, L1, R)):- K < X, split(L, K, F, L1). %left split

merge(T, nil, T):- !.

merge(nil, T, T):- !.

merge(node(X1, Val1, Y1, L1, R1), node(X2, Val2, Y2, L2, R2), node(X1, Val1, Y1, L1, Right)) :- Y2 >= Y1, merge(R1, node(X2, Val2, Y2, L2, R2), Right). %merge to right child

merge(node(X1, Val1, Y1, L1, R1), node(X2, Val2, Y2, L2, R2), node(X2, Val2, Y2, Left, R2)) :- Y2 < Y1, merge(node(X1, Val1, Y1, L1, R1), L2, Left). %merge to left child



map_get(node(X, Value, _, _, _), X, Value):-!.

map_get(node(X, _, _, L, R), Key, Value):- (Key < X, map_get(L, Key, Value)); (Key >= X, map_get(R, Key, Value)).


map_replace(Map, Key, Value, Result):- map_get(Map, Key, Val) , map_remove(Map, Key, T), map_put(T, Key, Value, Result), !.
map_replace(Map, Key, Value, Res):- Res = Map.
map_put(Tree, Key, Value, Res):-
	split(Tree, Key, L, R),
	split(L, Key - 1, L1, _),
	rand_int(1000, Rand),
	merge(L1, node(Key, Value, Rand, nil, nil), Res1),
	merge(Res1, R, Res).

merge(T, nil, T):- !.

merge(nil, T, T):- !.

merge(node(X1, Val1, Y1, L1, R1), node(X2, Val2, Y2, L2, R2), node(X1, Val1, Y1, L1, Right)) :- Y2 >= Y1, merge(R1, node(X2, Val2, Y2, L2, R2), Right). %merge to right child

merge(node(X1, Val1, Y1, L1, R1), node(X2, Val2, Y2, L2, R2), node(X2, Val2, Y2, Left, R2)) :- Y2 < Y1, merge(node(X1, Val1, Y1, L1, R1), L2, Left). %merge to left child
map_remove(Tree, Key, Res):- split(Tree, Key, L, R), split(L, Key - 1, L1, _), merge(L1, R, Res).

map_build([], nil).

map_build([(Key, Value) | T], Tree) :- map_build(T, Tree1), map_put(Tree1, Key, Value, Tree).