-module(p1to19_lists).
-include_lib("eunit/include/eunit.hrl").
-export([my_last/1, my_but_last/1, my_element_at/2, my_length/1, my_reverse/1, my_is_palindrome/1, my_flatten/1, my_compress/1, my_pack/1, my_encode/1,
         my_encode_modified/1, my_decode_modified/1, my_encode_direct/1, my_duplicate/1, my_replicate/2, my_drop/2, my_split/2, my_slice/3, my_rotate/2]).


%% Problem 1: Find the last element of a list.
my_last(List) -> lists:last(List).


%% Problem 2: Find the second last element of a list.
my_but_last(List) -> lists:nth(length(List) - 1, List).


%% Problem 3: Find the N'th element of a list. The first element in the list is number 1.
my_element_at(List, N) -> lists:nth(N, List).


%% Problem 4: Find the number of elements in a list.
my_length(List) -> lists:foldl(fun(_, Acc) -> Acc + 1 end, 0, List).


%% Problem 5: Reverse a list.
my_reverse(List) -> lists:foldl(fun(H, Acc) -> [H|Acc] end, [], List).


%% Problem 6: Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
my_is_palindrome(List) -> List == my_reverse(List).


%% Problem 7: Flatten a nested list structure.
my_flatten(List) -> lists:flatten(List).


%% Problem 8: If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
my_compress(List) -> lists:foldr(fun my_compress/2, [], List).
my_compress(H, Acc) ->
  case Acc of
    [H|T] -> [H|T];
    _ -> [H|Acc]
  end.


%% Problem 9: Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
my_pack(List) -> lists:foldr(fun my_pack/2, [], List).
my_pack(H, Acc) ->
  case Acc of
    [[H|SubT]|T] -> [[H,H|SubT]|T];
    _ -> [[H]|Acc]
  end.


%% Problem 10:  Run-length encoding of a list. Use the result of problem 9 to implement the so-called run-length encoding data compression method.
%%              Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
my_encode(List) -> lists:map(fun(H) -> {my_length(H), my_last(H)} end, my_pack(List)).


%% Problem 11: Modify problem 10 in such a way that if an element has no duplicates it is simply copied
%%             into the result list. Only elements with duplicates are transferred as (N E) lists.
my_encode_modified(List) -> lists:map(fun my_encode_modified_mapper/1, my_pack(List)).
my_encode_modified_mapper(H) ->
  case length(H) of
    1 -> hd(H);
    _ -> {length(H), hd(H)}
  end.


%% Problem 12: Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
my_decode_modified(List) -> lists:flatten(lists:map(fun my_decode_modified_mapper/1, List)).
my_decode_modified_mapper(H) ->
  case H of
    {N, E} -> lists:duplicate(N, E);
    _ -> H
  end.


%% Problem 13: Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create
%%             the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify
%%             the result list by replacing the singleton lists (1 X) by X.
my_encode_direct(List) -> lists:foldr(fun my_encode_direct/2, [], List).
my_encode_direct(H, Acc) ->
  case Acc of
    [] -> [H];
    [H|T] -> [{2, H}|T];
    [{N, H}|T] -> [{N + 1, H}|T];
    [H2|T] -> [H,H2|T]
  end.


%% Problem 14: Duplicate the elements of a list.
my_duplicate(List) -> lists:foldr(fun(H, Acc) -> [H,H|Acc] end, [], List).


%% Problem 15: Replicate the elements of a list N number of times.
my_replicate(List, N) -> lists:foldr(fun(H, Acc) -> lists:duplicate(N, H) ++ Acc end, [], List).


%% Problem 16: Drop every N'th element from a list.
my_drop(List, N) when (length(List) < N) -> List;
my_drop(List, N) ->
  {A, [_|T]} = lists:split(N-1, List),
  A ++ my_drop(T, N).


%% Problem 17: Split a list into two parts; the length of the first part is given as N.
my_split(List, N) -> lists:split(N, List).


%% Problem 18: Extract a slice from a list. Given two indices, I and K, the slice is the list containing the elements between
%%             the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.
my_slice(List, I, K) ->
  {_, B} = lists:split(I - 1, List),
  {C, _} = lists:split(K - I + 1, B),
  C.


%% Problem 19: Rotate a list N places to the left. Rotate to the right if N is negative. Use the result of problem 17.
my_rotate([], _) -> [];
my_rotate(List, N) ->
  L = length(List),
  {A, B} = if N > 0 -> my_split(List, N rem L);
              N < 0 -> my_split(List, L + (N rem L));
              true -> {0, List}
           end,
  B ++ A.