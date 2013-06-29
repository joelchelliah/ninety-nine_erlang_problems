-module(p20to30_lists).
-include_lib("eunit/include/eunit.hrl").
-export([my_remove_at/2, my_insert_at/3, my_range/2, my_rnd_select/2, my_lotto_select/2, my_rnd_permutation/1, my_combination/2, my_group3/1]).


%% Problem 20: Remove the N'th element from a list.
my_remove_at(List, N) ->
  {A, [_|T]} = lists:split(N-1, List),
  A ++ T.


%% Problem 21: Insert an element E at a given position N in a list.
my_insert_at(List, E, N) ->
  {A, B} = lists:split(N-1, List),
  A ++ [E|B].


%% Problem 22: Create a list containing all integers within a given range. If second argument is smaller than first, produce a list in decreasing order.
my_range(X1, X2) ->
  if X1 > X2 -> [X1|my_range(X1 - 1, X2)];
     X1 < X2 -> [X1|my_range(X1 + 1, X2)];
     true -> [X2]
  end.


%% Problem 23: Extract a given number of randomly selected elements from a list. The selected items shall be returned in a list.
my_rnd_select(_, 0) -> [];
my_rnd_select(List, N) ->
  Pos = random:uniform(length(List)),
  [lists:nth(Pos, List)|my_rnd_select(my_remove_at(List, Pos), N - 1)].


%% Problem 24: Draw N different random numbers from the set 1..M. The selected numbers shall be returned in a list.
my_lotto_select(N, M) -> my_rnd_select(my_range(1, M), N).


%% Problem 25: Generate a random permutation of the elements of a list.
my_rnd_permutation(List) -> my_rnd_select(List, length(List)).


%% Problem 26: Generate the combinations of K distinct objects chosen from the N elements of a list. For example: In how many
%%             ways can a committee of 3 be chosen from a group of 12 people? We want to generate all C(12,3) = 220 possibilities.
my_combination(0, _) -> [];
my_combination(1, List) -> lists:map(fun(X) -> [X] end, List);
my_combination(N, List) -> my_combination(N, List, List, []).
my_combination(_, [], _, Acc) -> Acc;
my_combination(N, [H|T], Sub, Acc) ->
  if (length(Sub) < N) -> my_combination(N, T, T, Acc);
     true ->
       {A, B} = lists:split(N-1, Sub),
       if length(A) == 1 -> my_combination(N, T, T, Acc ++ generate_combinations(A, B));
          true -> my_combination(N, [H|T], my_remove_at(Sub, 2), Acc ++ generate_combinations(A, B))
       end
   end.

generate_combinations(_, []) -> [];
generate_combinations(Base, [H|T]) -> [lists:sort([H|Base]) | generate_combinations(Base, T)].


%% Problem 27: How many ways can you group the elements of a set of size 9 into 3 disjoint subsets of sizes 2,3 and 4? Write a function that generates
%%             all the possibilities and returns them in a list. Note that we do not want permutations of the group members; i.e. [[a, b], ...] is the
%%             same solution as [[b, a], ...]. However, we make a difference between [[a, b], [c, d, ...], ...] and [[c, d], [a, b, ...], ...].
my_group3(List) ->
  Twos = my_combination(2, List),
  Threes = my_combination(3, List),
  Fours = lists:map(fun(X) -> [X] end, my_combination(4, List)),
  ThreesAndFours = lists:foldr(fun(H, Acc) -> update_subgroups(H, Fours) ++ Acc end, [], Threes),
  lists:foldr(fun(H, Acc) -> update_subgroups(H, ThreesAndFours) ++ Acc end, [], Twos).

update_subgroups(Set, DisjointSubgroups) ->
  lists:foldr(fun(H, Acc) ->
    case lists:any(fun(S) -> lists:any(fun(E) -> lists:member(E, Set) end, S) end, H) of
      true -> Acc;
      false -> [lists:append([Set],H) | Acc]
    end
  end, [], DisjointSubgroups).