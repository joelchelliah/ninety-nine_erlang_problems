-module(p20to30_lists).
-include_lib("eunit/include/eunit.hrl").
-export([my_remove_at/2, my_insert_at/3, my_range/2, my_rnd_select/2, my_lotto_select/2, my_rnd_permutation/1, my_combination/2,
         my_group3/1, my_group3_generalized/2, my_lsort/1, my_lfsort/1, times_member/2]).


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
  if X1 > X2 -> lists:seq(X1, X2, -1);
     true -> lists:seq(X1, X2)
  end.


%% Problem 23: Extract a given number of randomly selected elements from a list. The selected items shall be returned in a list.
my_rnd_select(_, 0) -> [];
my_rnd_select(List, N) ->
  Pos = random:uniform(length(List)),
  [lists:nth(Pos, List)|my_rnd_select(my_remove_at(List, Pos), N - 1)].


%% Problem 24: Draw N different random numbers from the set 1..M. The selected numbers shall be returned in a list.
my_lotto_select(N, M) -> my_rnd_select(lists:seq(1, M), N).


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
       Combinations = generate_combinations(A, B),
       if length(A) == 1 -> my_combination(N, T, T, Acc ++ Combinations);
          true -> lists:foldr(fun(Remove, Results) ->
                                lists:umerge(Results, my_combination(N, [H|T], my_remove_at(Sub, Remove), Acc ++ Combinations))
                              end, [], lists:seq(2,length(A)))
       end
   end.

generate_combinations(_, []) -> [];
generate_combinations(Base, [H|T]) -> [lists:sort([H|Base]) | generate_combinations(Base, T)].


%% Problem 27: How many ways can you group the elements of a set of size 9 into 3 disjoint subsets of sizes 2,3 and 4? Write a function that generates
%%             all the possibilities and returns them in a list. Note that we do not want permutations of the group members; i.e. [[a, b], ...] is the
%%             same solution as [[b, a], ...]. However, we make a difference between [[a, b], [c, d, ...], ...] and [[c, d], [a, b, ...], ...].
my_group3(List) ->
  [Twos, Threes, Fours] = lists:foldr(fun(H, Acc) -> [my_combination(H, List) | Acc] end, [], [2,3,4]),
  FoursGrouped = lists:map(fun(X) -> [X] end, Fours),
  lists:foldr(fun(SubGroupList, DisjointSubgroups) ->
                  lists:foldr(fun(SubGroup, Acc) -> update_subgroups(SubGroup, DisjointSubgroups) ++ Acc end, [], SubGroupList)
                end, FoursGrouped, [Twos, Threes]).

update_subgroups(SubGroup, DisjointSubgroups) ->
  lists:foldr(fun(H, Acc) ->
                case lists:any(fun(S) -> lists:any(fun(E) -> lists:member(E, SubGroup) end, S) end, H) of
                  true -> Acc;
                  false -> [[SubGroup] ++ H | Acc]
                end
              end, [], DisjointSubgroups).


%% Problem 28: Generalize the above function in a way that we can specify a list of subset sizes. The number of disjoint subsets in each group should be equal
%%             to length of the list of subset sizes.
my_group3_generalized(List, Sizes) ->
  Subsets = lists:foldr(fun(H, Acc) -> [my_combination(H, List) | Acc] end, [], Sizes),
  {Ungrouped, [Last]} = lists:split(length(Subsets) - 1, Subsets),
  Grouped = lists:map(fun(X) -> [X] end, Last),
  lists:foldr(fun(SubGroupList, DisjointSubgroups) ->
                  lists:foldr(fun(SubGroup, Acc) -> update_subgroups(SubGroup, DisjointSubgroups) ++ Acc end, [], SubGroupList)
              end, Grouped, Ungrouped).


%% Problem 29: We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to
%%             their length. E.g. short lists first, longer lists later, or vice versa.
my_lsort(List) -> lists:sort(fun(A, B) -> length(A) < length(B) end, List).


%% Problem 30: Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the
%%             elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly,
%%             lists with rare lengths are placed first, others with a more frequent length come later.
my_lfsort(ListOfLists) ->
  Lengths = lists:map(fun(X) -> length(X) end, ListOfLists),
  lists:sort(fun(A, B) -> times_member(length(A), Lengths) < times_member(length(B), Lengths) end, ListOfLists).

times_member(X, Xs) ->
  lists:foldl(fun(H, Acc) ->
                case X == H of true -> Acc + 1; false -> Acc end
              end, 0, Xs).