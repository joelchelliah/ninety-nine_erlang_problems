# ninety-nine_erlang_problems #


#### Based on [L-99: Ninety-Nine Lisp Problems](http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html) ####

My goal is to solve (or at least try to solve) all 99 problems, in Erlang!
Will be adding more descriptions and Erlang examples of the problems to the list below, as I work my way through them.

**Current status:** 13/99

---


**Problem 1**

Find the last element of a list.
```erl
1> p_1_to_10:my_last([1,2,3,4]).
4
2> p_1_to_10:my_last([a,b,c,d]).
d
```

---

**Problem 2**

Find the second last element of a list.
```erl
1> p_1_to_10:my_but_last([1,2,3,4]).
3
2> p_1_to_10:my_but_last([a,b,c,d]).
c
```

---

**Problem 3**

Find the N'th element of a list. The first element in the list is number 1.
```erl
1> p_1_to_10:my_element_at([a,b,c,d,e,f], 3).
c
2> p_1_to_10:my_element_at([1,2,3,1,2,3,1,2,3], 8).
2
```

---

**Problem 4**

Find the number of elements in a list.
```erl
1> p_1_to_10:my_length([]).
0
2> p_1_to_10:my_length([a,b,c,d,e]).
5
```

---

**Problem 5**

Reverse a list.
```erl
1> p_1_to_10:my_reverse([]).
[]
2> p_1_to_10:my_reverse([1,2,3,4,5]).
[5,4,3,2,1]
3> p_1_to_10:my_reverse("!namtaB").
"Batman!"
```

---

**Problem 6**

Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
```erl
1> p_1_to_10:my_is_palindrome([1,2,3,4,5,4,3,2,1]).
true
2> p_1_to_10:my_is_palindrome("racecar").
true
3> p_1_to_10:my_is_palindrome([a,b,c,a]).
false
```

---

**Problem 7**

Flatten a nested list structure.
```erl
1> p_1_to_10:my_flatten([[[1],[a]]]).
[1,a]
2> p_1_to_10:my_flatten([1,[2, [3, [a], b], c], d]).
[1,2,3,a,b,c,d]
3> p_1_to_10:my_flatten(["Hello",[" ", ["World"]]]).
"Hello World"
```

---

**Problem 8**

If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
```erl
1> p_1_to_10:my_compress([a,a,a,b,c,c,c,d,d,a,b,b,b,b]).
[a,b,c,d,a,b]
2> p_1_to_10:my_compress("hiiiiiii!!!!! :::::D").
"hi! :D"
```

---

**Problem 9**

Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
```erl
1> p_1_to_10:my_pack([a,a,a,b,c,c,c,d,d,a,b,b,b,b]).
[[a,a,a],[b],[c,c,c],[d,d],[a],[b,b,b,b]]
2> p_1_to_10:my_pack([1,"one","one",1,1,1,"one",1]).
[[1],["one","one"],[1,1,1],["one"],[1]]
```

---

**Problem 10**

Run-length encoding of a list. Use the result of problem 9 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples {N E} where N is the number of duplicates of the element E.
```erl
1> p_1_to_10:my_encode([a,a,a,b,c,c,c,d,d,a,b,b,b,b]).
[{3,a},{1,b},{3,c},{2,d},{1,a},{4,b}]
2> p_1_to_10:my_encode([1,"one","one",1,1,1,"one",1]).
[{1,1},{2,"one"},{3,1},{1,"one"},{1,1}]
```

---

**Problem 11**

Modify problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as {N E} tuples.
```erl
1> p_11_to_20:my_encode_modified([a,a,a,b,c,c,c,d,d,a,b,b,b,b]).
[{3,a},b,{3,c},{2,d},a,{4,b}]
2> p_11_to_20:my_encode_modified([1,"one","one",1,1,1,"one",1]).
[1,{2,"one"},{3,1},"one",1]
```

---

**Problem 12**

Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
```erl
1> p_11_to_20:my_decode_modified([{3,a},b,{3,c},{2,d},a,{4,b}]).
[a,a,a,b,c,c,c,d,d,a,b,b,b,b]
2> p_11_to_20:my_decode_modified([{0, 1},{0, 2},{0,3}]).
[]
```

---

**Problem 13**

Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem 11, simplify the result list by replacing the tuples {1 X} by X.
Find the second last element of a list.
```erl
1> p_11_to_20:my_encode_direct([a,a,a,b,c,c,c,d,d,a,b,b,b,b]).
[{3,a},b,{3,c},{2,d},a,{4,b}]
2> p_11_to_20:my_encode_direct([1,"one","one",1,1,1,"one",1]).
[1,{2,"one"},{3,1},"one",1]
```



.

.

.

WIP! (｡◕‿◕｡)