
val listInt = [1,2,3,4,5,6,7,8,9,10];
val randoInt = [7,38,59,24,71,84,50,10,2]
(*Q1*)
fun min3(x, y, z) =
  if x < y then
      if x < z then x
      else z
  else if y < z then y
  else z
;

min3(10,5,2);
(*Q2*)
fun getLength(list) =
  if null list then 0
    else 1 + length (tl list);

getLength(listInt);

(*Q3*)
fun cycle(x::xs, y) =
  if y = 0 then x::xs
  else
  cycle(xs @ [x], y-1)
;

cycle(listInt, 3);

(*Q4*)
fun isPrime 0 = false
  | isPrime 1 = false
  | isPrime 2 = true
  | isPrime num =
    let
      fun divide(dividend, 1) = true
        | divide (dividend, divisor) =
            if dividend mod divisor = 0 then false
            else
            divide(dividend, divisor-1)
    in
      divide(num, num-1)
    end
;

isPrime 14;

(*Q5*)
fun select(x::xs, isPrime) =
    if xs = nil
      then
        if isPrime(x) = false then []
        else [x]
    else if isPrime(x) = false then select(xs, isPrime)
    else
      select(xs, isPrime) @ [x]
;

select([1,2,3,4,5,6,7,8,9,10], isPrime);

(*Q6*)
fun band([]) = true
  |band(x::xs) =
    if xs = nil then x
    else if x = true then true
    else band(xs)
;

val boollist = [false, false,false];

band(boollist);

(*Q7*)
fun dupList([]) = []
  |dupList(x::xs) = x:x::dupList(xs);

dupList(listInt);

fun max([]) = ~1
  | max(x::xs) =
  if xs = [] then x
  else if x > max(xs) then x
  else max(xs);

max(randoInt);

(*Q9*)
fun makeBST([]) = Empty
  | makeBST(x::xs) =
  let
      fun insert(x, Empty) = Node(Empty, x, Empty)
      |insert(x, Node(leftNode, value, rightNode)) =
      case x < value of
        true => Node(insert(x, leftNode), value, rightNode)|(*Left*)
        false => Node(leftNode, value, insert(x, rightNode)) | (*Right*)
        _ => Node(leftNode,value,rightNode)
  in
  insert(x, makeBST(xs))
  end;


val theTree = makeBST(randoInt);

(*Q10*)
fun searchBST(findInt, Empty) = false
  | searchBST(findInt, Node(leftNode, value, rightNode)) =
    if findInt = value then true
    else if findInt < value then searchBST(findInt, leftNode)
    else  searchBST(findInt, rightNode)
;

searchBST(59, theTree);
