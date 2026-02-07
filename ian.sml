
1.
fun smallestInt (int1, int2, int3) = if int1 < int2 andalso int1 < int3 then int1 else if int2 < int1 andalso int2 < int3
then int2 else int3;

2.
fun cycle [] _ = []
  | cycle list 0 =  list
  | cycle (x::xs) num = cycle (xs @ [x]) (num - 1);

3.
fun checkDivisor num divisor = if divisor * divisor > num then true else num mod divisor <> 0 andalso checkDivisor num (divisor + 1);

fun isPrime num = if num < 2 then false else checkDivisor num 2;

4.
fun select f [] = []
  | select f (x::xs) = if f x then x :: (select f xs) else select f xs;

5.
fun band [] = true
  | band [x] = x
  | band (x::xs) = x orelse band xs;

6.
fun dupList [] = []
  | dupList [x] = [x, x]
  | dupList (x::xs) = [x, x] @ dupList (xs);

7.
fun max [] = 0
  | max [x] = x
  | max (x::xs) = if x > max (xs) then x else max (xs);

8.
fun convert L = foldr (fn ((x, y), (L1, L2)) => (x::L1, y::L2)) ([], []) L;

9.
datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree;

fun makeBST [] = Empty
  | makeBST (x::xs) =
    let
      fun insert Empty x = Node (Empty, x, Empty)
        | insert (Node (left, value, right)) x =
           if x < value then Node (insert left x, value, right)
           else Node (left, value, insert right x)
      in
        foldl (fn (item, tree) => insert tree item) Empty (x::xs)
End;

10.
fun searchBST Empty key = NONE
  | searchBST (Node(left, value, right)) key =
    if key < value then searchBST left key
    else if key > value then searchBST right key
    else SOME value;
