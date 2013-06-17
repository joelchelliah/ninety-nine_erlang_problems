-module(p_11_to_20).
-include_lib("eunit/include/eunit.hrl").
-import(probs_1_to_10, [my_pack/1]).
-export([my_encode_modified/1, my_decode_modified/1, my_encode_direct/1]).

%% Problem 11: Modify the result of problem 10 in such a way that if an element has no duplicates it is simply
%%             copied into the result list. Only elements with duplicates are transferred as (N E) lists.
my_encode_modified(List) ->
    Encode = fun(H) ->
                case length(H) of
                    1 -> hd(H);
                    _ -> {length(H), hd(H)}
                end
            end,
    lists:map(Encode, my_pack(List)).


%% Problem 12: Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
my_decode_modified(List) ->
    Decode = fun(H) ->
                case H of
                    {N, E} -> lists:duplicate(N, E);
                    _ -> H
                end
             end,
    lists:flatten(lists:map(Decode, List)).


%% Problem 13: Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create
%%             the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify
%%             the result list by replacing the singleton lists (1 X) by X.
my_encode_direct(List) ->
    Encode = fun(H, Acc) ->
                case Acc of
                    [] -> [H];
                    [H|T] -> [{2, H}|T];
                    [{N, H}|T] -> [{N + 1, H}|T];
                    [H2|T] -> [H,H2|T]
                end
             end,
    lists:foldr(Encode, [], List).


%% Problem 14: