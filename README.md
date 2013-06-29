# ninety-nine_erlang_problems #


#### Based on [L-99: Ninety-Nine Lisp Problems](http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html) ####

I'm making some slight changes to the problem descriptions to make them fit more to Erlang code, but they should still be pretty similar to the original Lisp problems.
Will be adding more problems and examples to the list below, as I work my way through them.

**Current status:** 27/99
</br></br>

---

#### List Problems (1-30): ####

---

**Problem 1**  
Find the last element of a list.
```erl
1> p1to19_lists:my_last([1,2,3,4]).
4
```

---

**Problem 2**  
Find the second last element of a list.
```erl
1> p1to19_lists:my_but_last([1,2,3,4]).
3
```

---

**Problem 3**  
Find the N'th element of a list. The first element in the list is number 1.
```erl
1> p1to19_lists:my_element_at([a,b,c,d,e,f], 3).
c
```

---

**Problem 4**  
Find the number of elements in a list.
```erl
1> p1to19_lists:my_length([a,b,c,d,e]).
5
```

---

**Problem 5**  
Reverse a list.
```erl
1> p1to19_lists:my_reverse([1,2,3,4,5]).
[5,4,3,2,1]
```

---

**Problem 6**  
Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
```erl
1> p1to19_lists:my_is_palindrome([1,2,3,4,5,4,3,2,2]).
false
2> p1to19_lists:my_is_palindrome("racecar").
true
```

---

**Problem 7**  
Flatten a nested list structure.
```erl
1> p1to19_lists:my_flatten([1,[2, [3, [a], b], c], d]).
[1,2,3,a,b,c,d]
2> p1to19_lists:my_flatten(["Hello",[" ", ["World"]]]).
"Hello World"
```

---

**Problem 8**  
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
```erl
1> p1to19_lists:my_compress("hiiiiiii!!!!! :::::D").
"hi! :D"
```

---

**Problem 9**  
Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
```erl
1> p1to19_lists:my_pack([a,a,a,b,c,c,c,d,d,a,b,b,b,b]).
[[a,a,a],[b],[c,c,c],[d,d],[a],[b,b,b,b]]
```

---

**Problem 10**  
Run-length encoding of a list. Use the result of problem 9 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples {N E} where N is the number of duplicates of the element E.
```erl
1> p1to19_lists:my_encode([a,a,a,b,c,c,c,d,d,a,b,b,b,b]).
[{3,a},{1,b},{3,c},{2,d},{1,a},{4,b}]
```

---

**Problem 11**  
Modify problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as {N E} tuples.
```erl
1> p1to19_lists:my_encode_modified([a,a,a,b,c,c,c,d,d,a,b,b,b,b]).
[{3,a},b,{3,c},{2,d},a,{4,b}]
```

---

**Problem 12**  
Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
```erl
1> p1to19_lists:my_decode_modified([{3,a},b,{3,c},{2,d},a,{4,b}]).
[a,a,a,b,c,c,c,d,d,a,b,b,b,b]
```

---

**Problem 13**  
Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem 11, simplify the result list by replacing the tuples {1 X} by X.
Find the second last element of a list.
```erl
1> p1to19_lists:my_encode_direct([a,a,a,b,c,c,c,d,d,a,b,b,b,b]).
[{3,a},b,{3,c},{2,d},a,{4,b}]
```

---

**Problem 14**  
Duplicate the elements of a list.
```erl
1> p1to19_lists:my_duplicate([a,b,c,c,d]).
[a,a,b,b,c,c,c,c,d,d]
```

---

**Problem 15**  
Replicate the elements of a list N number of times.
```erl
1> p1to19_lists:my_replicate([a,b,c,c,d], 3).
[a,a,a,b,b,b,c,c,c,c,c,c,d,d,d]
```

---

**Problem 16**  
Drop every N'th element from a list.
```erl
1> p1to19_lists:my_drop("nannannannan Baattmaan", 3).
"nananana Batman"
```

---

**Problem 17**  
Split a list into two parts; the length of the first part is given as N.
```erl
1> p1to19_lists:my_split([1,2,3,4,5,6,7,8,9], 4).
{[1,2,3,4],[5,6,7,8,9]}
```

---

**Problem 18**  
Extract a slice from a list. Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.
```erl
1> p1to19_lists:my_slice([a,b,c,d,e,f,g], 3, 6).
[c,d,e,f]
```

---

**Problem 19**  
Rotate a list N places to the left. Rotate to the right if N is negative. Use the result of problem 17.
```erl
1> p1to19_lists:my_rotate([a,b,c,d,e,f,g], 3).  
[d,e,f,g,a,b,c]
2> p1to19_lists:my_rotate([a,b,c,d,e,f,g], -2).
[f,g,a,b,c,d,e]
```

---

**Problem 20**  
Remove the N'th element from a list.
```erl
1> p20to30_lists:my_remove_at([a,b,c,d,e,f,g], 3).
[a,b,d,e,f,g]
```

---

**Problem 21**  
Insert an element E at a given position N in a list.
```erl
1> p20to30_lists:my_insert_at([a,b,c,d,e], "Boo!", 3 ).
[a,b,"Boo!",c,d,e]
```

---

**Problem 22**  
Create a list containing all integers within a given range. If second argument is smaller than first, produce a list in decreasing order.
```erl
1> p20to30_lists:my_range(3,19).
[3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
2> p20to30_lists:my_range(3,-4).
[3,2,1,0,-1,-2,-3,-4]
```

---

**Problem 23**  
Extract a given number of randomly selected elements from a list. The selected items shall be returned in a list.
```erl
1> p20to30_lists:my_rnd_select([a,b,c,d,e,f,g], 3).
[d,f,g]
```

---

**Problem 24**  
Draw N different random numbers from the set 1..M. The selected numbers shall be returned in a list.
```erl
1> p20to30_lists:my_lotto_select(7, 100).          
[92,67,47,59,14,21,71]
```

---

**Problem 25**  
Generate a random permutation of the elements of a list.
```erl
1> p20to30_lists:my_rnd_permutation([a,b,c,d,e,f,g,h]).
[b,e,c,f,d,a,h,g]
```

---

**Problem 26**  
Generate the combinations of K distinct objects chosen from the N elements of a list. For example: In how many ways can a committee of 3 be chosen from a group of 12 people? We want to generate all C(12,3) = 220 possibilities.
```erl
1> p20to30_lists:my_combination(3, [a,b,c,d]).  
[[a,b,c],[a,b,d],[a,c,d],[b,c,d]]
```

---

**Problem 27**  
How many ways can you group the elements of a set of size 9 into 3 disjoint subsets of sizes 2,3 and 4? Write a function that generates all the possibilities and returns them in a list. Note that we do not want permutations of the group members; i.e. [[a, b], ...] is the same solution as [[b, a], ...]. However, we make a difference between [[a, b], [c, d, ...], ...] and [[c, d], [a, b, ...], ...].
```erl
14> p20to30_lists:my_group3([a,b,c,d,e,f,g,h,i]).
[[[a,b],[c,d,e],[f,g,h,i]],
 [[a,b],[c,d,f],[e,g,h,i]],
 [[a,b],[c,d,h],[e,f,g,i]],
 [[a,b],[c...
 ...
```


.

.

.


</br></br>

---

#### Arithmetic Problems (31-41):  ####

---

.

.

.

WIP! (｡◕‿◕｡)