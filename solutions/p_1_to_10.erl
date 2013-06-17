-module(p_1_to_10).
-export([my_last/1, my_but_last/1, my_element_at/2, my_length/1, my_reverse/1, my_is_palindrome/1, my_flatten/1, my_compress/1, my_pack/1, my_encode/1]).


%% Problem 1: Find the last element of a list.
my_last([H|[]]) -> H;
my_last([_|T]) -> my_last(T).


%% Problem 2: Find the second last element of a list.
my_but_last([H|[_|[]]]) -> H;
my_but_last([_|T]) -> my_but_last(T).


%% Problem 3: Find the N'th element of a list. The first element in the list is number 1.
my_element_at([H|_], 1) -> H;
my_element_at([_|T], N) when N > 1 -> my_element_at(T, N - 1).


%% Problem 4: Find the number of elements in a list.
my_length(List) -> lists:foldl(fun(_, Acc) -> Acc + 1 end, 0, List).


%% Problem 5: Reverse a list.
my_reverse(List) -> lists:foldl(fun(H, Acc) -> [H|Acc] end, [], List).


%% Problem 6: Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
my_is_palindrome(List) -> List == my_reverse(List).


%% Problem 7: Flatten a nested list structure.
my_flatten([]) -> [];
my_flatten([H|T]) when is_list(H) -> my_flatten(H) ++ my_flatten(T);
my_flatten([H|T]) -> [H|my_flatten(T)].


%% Problem 8: If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
my_compress(List) -> lists:foldr(fun my_compress/2, [], List).
my_compress(H, Acc) ->
    case Acc of
        [H|T] -> [H|T];
        [H2|T] -> [H,H2|T];
        _ -> [H]
    end.


%% Problem 9: Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
my_pack(List) -> lists:foldr(fun my_pack/2, [], List).
my_pack(H, Acc) ->
    case Acc of
        [[H|SubT]|T] -> [[H,H|SubT]|T];
        [Sub|T] -> [[H],Sub|T];
        _ -> [[H]]
    end.


%% Problem 10:  Run-length encoding of a list. Use the result of problem 9 to implement the so-called run-length encoding data compression method.
%%              Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
my_encode(List) -> lists:map(fun(H) -> {my_length(H), my_last(H)} end, my_pack(List)).