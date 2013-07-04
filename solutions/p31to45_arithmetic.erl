-module(p31to45_arithmetic).
-include_lib("eunit/include/eunit.hrl").
-export([my_is_prime/1, my_gcd/2, my_is_coprime/2, my_totient_phi/1, my_prime_factors/1, my_prime_factors_mult/1, my_totient_phi_improved/1,
         my_totient_compare/1, my_primes_range/2, my_goldbach/1, my_goldbach_list/2, my_goldbach_list/3]).


%% Problem 31: Determine whether a given integer number is prime.
my_is_prime(N) when N < 3 -> N == 2;
my_is_prime(N) -> lists:foldr(fun(H, Acc) -> Acc and (N rem H =/= 0) end, true, lists:seq(2, N-1)).


%% Problem 32: Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
my_gcd(X, Y) when Y > X -> my_gcd(Y, X);
my_gcd(X, Y) ->
  R = X rem Y,
  case R of
    0 -> Y;
    _ -> my_gcd(Y, R)
  end.


%% Problem 33: Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
my_is_coprime(X, Y) -> my_gcd(X, Y) == 1.


%% Problem 34: Calculate Euler's totient function phi(m). Euler's so-called totient function phi(m) is defined
%%             as the number of positive integers r (1 <= r < m) that are coprime to m.
my_totient_phi(1) -> 1;
my_totient_phi(N) -> my_totient_phi(N, lists:seq(1, N-1)).

my_totient_phi(N, List) ->
  F = fun(X, Acc) -> case my_is_coprime(N, X) of
                       true -> 1 + Acc;
                       false -> Acc
                     end end,
  lists:foldl(F, 0, List).


%% Problem 35: Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
my_prime_factors(N) -> my_prime_factors(N, 2).

my_prime_factors(N, M) when (N < 4) or (M == N) -> [N];
my_prime_factors(N, M) ->
  case N rem M of
    0 -> [M | my_prime_factors(N div M, 2)];
    _ -> my_prime_factors(N, M + 1)
  end.


%% Problem 36: Given a positive integer, construct a list containing the prime factors and their multiplicity.
my_prime_factors_mult(N) ->
  F = fun(H, Acc) -> case Acc of
                       [{H, Times}|T] -> [{H, Times + 1}|T];
                       _ -> [{H, 1} | Acc]
                     end end,
  lists:foldr(F, [], my_prime_factors(N)).


%% Poblem 37: Calculate Euler's totient function phi(m) (improved). See problem 34 for the definition of Euler's totient function.
%%            If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be
%%            efficiently calculated as follows: Let [{p1, m1}, {p2, m2}, {p3, m3}, ...] be the list of prime factors (and their multiplicities)
%%            of a given number m. Then phi(m) can be calculated with the following formula:
%%            phi(m) = (p1 - 1) * p1^(m1 - 1) * (p2 - 1) * p2^(m2 - 1) * (p3 - 1) * p3^(m3 - 1) * ...
my_totient_phi_improved(1) -> 1;
my_totient_phi_improved(N) ->
  round(lists:foldl(fun(H, Acc) -> H * Acc end,
                    1,
                    lists:map(fun({P, M}) -> (P - 1) * math:pow(P, M - 1) end,
                              my_prime_factors_mult(N)))).


%% Problem 38: Compare the two methods of calculating Euler's totient function. Use the solutions of problems 34 and 37 to compare the algorithms.
%%             Take the number of reductions as a measure for efficiency. Try to calculate phi(10090) as an example.
my_totient_compare(N) ->
  Ans = case my_totient_phi(N) == my_totient_phi_improved(N) of
          true -> "It works!";
          false -> "It doesn't work."
        end,
  io:format("      `_`~n"),
  io:format("     (o,o)  ~s~n", [Ans]),
  io:format("     {'\"'}~n"),
  io:format("   ===\"=\"=======~n").


%% Problem 39: Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
my_primes_range(L, H) when L > H -> [];
my_primes_range(L, H) ->
  case my_is_prime(L) of
    true -> [L | my_primes_range(L + 1, H)];
    false -> my_primes_range(L + 1, H)
  end.


%% Problem 40: Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
%%             Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case.
%%             It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system).
%%             Write a function to find the two prime numbers that sum up to a given even integer.
my_goldbach(N) ->
  Primes = my_primes_range(2, N),
  Pairs = lists:map(fun(X) -> {X, N - X} end, Primes),
  lists:nth(1, lists:filter(fun({_, X2}) -> my_is_prime(X2) end, Pairs)).


%% Problem 41: Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
my_goldbach_list(L, H) when L > H -> [];
my_goldbach_list(L, H) when L < 3 -> my_goldbach_list(L + 1, H);
my_goldbach_list(L, H) ->
  case L rem 2 of
    0 -> [{L, my_goldbach(L)} | my_goldbach_list(L + 1, H)];
    _ -> my_goldbach_list(L + 1, H)
  end.


%% Problem 42: In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely,
%%             the primes are both bigger than N. Try to find out how many such cases there are in the range L...H.
my_goldbach_list(L, H, N) ->
  GoldbachList = my_goldbach_list(L, H),
  lists:filter(fun({_, {X1, X2}}) -> (X1 >= N) and (X2 >= N) end, GoldbachList).