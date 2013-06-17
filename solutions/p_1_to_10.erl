-module(p_1_to_10).
-include_lib("eunit/include/eunit.hrl").
-export([my_last/1, my_but_last/1, element_at/2, my_length/1, my_reverse/1, is_palindrome/1, my_flatten/1, my_compress/1, my_pack/1, my_encode/1]).


%% Problem 1: Find the last element of a list.
my_last([H|[]]) -> H;
my_last([_|T]) -> my_last(T).


%% Problem 2: Find the second last element of a list.
my_but_last([H|[_|[]]]) -> H;
my_but_last([_|T]) -> my_but_last(T).


%% Problem 3: Find the N'th element of a list. The first element in the list is number 1.
element_at([H|_], 1) -> H;
element_at([_|T], N) when N > 1 -> element_at(T, N - 1);


%% Problem 4: Find the number of elements in a list.
my_length([]) -> 0;
my_length([_|T]) -> 1 + my_length(T).


%% Problem 5: Reverse a list.
my_reverse([]) -> [];
my_reverse(List) ->
    Rev = fun(_, [], Acc) -> Acc;
             (F, [H|T], Acc) -> F(F, T, [H|Acc])
          end,
    Rev(Rev, List, []).


%% Problem 6: Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
is_palindrome(List) -> List == my_reverse(List).


%% Problem 7: Flatten a nested list structure.
my_flatten([]) -> [];
my_flatten([H|T]) when is_list(H) -> my_flatten(H) ++ my_flatten(T);
my_flatten([H|T]) -> [H|my_flatten(T)].


%% Problem 8: Eliminate consecutive duplicates of list elements.
my_compress([]) -> [];
my_compress([H|[]]) -> [H];
my_compress([H1|[H2|T]]) ->
    if H1 =:= H2 -> my_compress([H2|T]);
        true -> [H1|my_compress([H2|T])]
    end.


%% Problem 9: Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
my_pack(List) ->
    Pack = fun(_, [], Acc) -> Acc;
              (F, [H|T], []) -> F(F, T, [[H]]);
              (F, [H|T], [AH|AT]) ->
                  case AH of
                      [H|_] -> F(F, T, [[H|AH]|AT]);
                      _ -> F(F, T, [[H],AH|AT])
                  end
          end,
    my_reverse(Pack(Pack, List, [])).


%% Problem 10:  Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method.
%%              Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
my_encode(List) -> lists:map(fun(H) -> {my_length(H), my_last(H)} end, my_pack(List)).