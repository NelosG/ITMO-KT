

nth_prime(N, RES):- N1 is N * 6, mass(N1,R), finN(N,R, RES).
finN(1, [H | T], RES):- RES = H.
finN(N, [H | T], RES):- N1 is N - 1,finN(N1, T, RES).

unique_prime_divisors(N, Divisors):- prime_divisors(N, R), del(0, R,[], Divisors).

del(Pred, [], K, RES):- RES = K.
del(Pred, [H | T], K, RES):- (H > Pred, H1 = [H], append(K, H1, K1), del(H, T,K1, RES)) ; (H = Pred, del(H, T, K, RES)).
 
heck(N, L):- L > sqrt(N) .
heck(N, L):- N1 is N mod L, N1 \= 0, L1 is L + 1, heck(N, L1).
prime(N) :- heck(N, 2).


composite(N):- not(prime(N)).
kol(N, [], RES):- RES = N.
kol(N, [H|T], RES):- N1 is N + 1, kol(N1, T, RES).


% N - число
prime_divisors(N, Divisors):- number(N), !, check(N, Divisors).
%массив не дан(ищем)
check(N, Divisors):- var(Divisors), !, find(N, [], K), Divisors = K.
find(1,K, RES):- RES = K.
find(N,K, RES):- keep(N, 2, T, R), append(K, R, K1), find(T, K1, RES).
keep(N, L, T, R):- N >= L, N1 is N mod L, ((N1 \= 0, L1 is L + 1, keep(N, L1, T, R)); (N1 = 0, T is N / L, R = [L])). 


%массив дан(проверяем)
check(N, Divisors):- list(Divisors), !, mult(1, 2,Divisors, K), K = N.
mult(R, Pred, [], K):- K = R.
mult(R, Pred, [H | T], K):- K1 is R * H, Pred =< H, mult(K1, H, T, K).

%N-перменная, находим N и проверяем
prime_divisors(N, Divisors):- var(N), !, findN(N, 1, Divisors), check(N, Divisors).

findN(N, K, []):- N = K.
findN(N, K, [H | T]):-  K1 is K * H, findN(N, K1, T).
%можно обойтись findN и функцией построения массива, но так дольше

%iskl(K, [], R, RES):- RES = R.
%iskl(K, [H | T], R, RES):- H1 is H mod K, (H1 \= 0,H2 = [H], append(R, H2, R1), iskl(K, T, R1, RES), !); (H1 = 0, iskl(K, T, R, RES), !).
%resheto([], K, RES):- RES = K.
%resheto([H | T], K, RES):- H1 = [H], iskl(H, T, [], R), append(K, H1, R1), resheto(R, R1, RES). 

%create(N, R, RES):- N < 2, RES = R.
%create(N, R, RES):- \+ (N < 2), N1 is N - 1, N2 = [N], append(N2, R, R1), create(N1, R1, RES) .

%mass(N, RES):- create(N, [], R), resheto(R, [],RES).
