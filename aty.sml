
(*Aty, cs 362*)

(*1 min3 of type int * int * int -> int *)
fun min3 (a,b,c)=
if a<=b andalso b<=c then a
else if b<=c andalso c<=a then b
else c;
min3(4,5,6);

(*2 cycle of type ‘a list * int -> ‘a list  *)
fun cycle([],_)=[]
  |cycle (a,0)=a
  |cycle (x::xs,n)=cycle(xs @ [x],n-1);
cycle([1,2,3,4],2);

(*3 isPrime of type int -> bool *)
fun isPrime a=
if a <= 1  then false
else if a=2  then true
else if  a mod 2=0 then false
else let
      fun check d=
      if d*d > a  then true
      else if a mod d = 0 then false
      else check (d+2)
    in
      check (3)
    end;
isPrime 15;

(*4 select of this type:
‘a list * (‘a -> bool) -> ‘a list
*)
fun select ([],_)=[]
  |select (x::xs,f)=
    if f x = true then x:: select (xs,f)
    else select (xs,f);
select([1,2,3,4,5,6,7,8,9,10], isPrime);


(*#5 band of type bool list -> bool *)
fun band([])=true
   |band (x::xs)=
        if x then true
        else band xs;
band[];
band [false, false, true, false];

(*#6 dupList of type ‘a list -> ‘a list *)
fun dupList []=[]
  | dupList (x::xs)= x::x:: dupList xs;
dupList[1,2,3];

(*#7 max of type int list -> int that returns the largest element *)
fun max [x]= x
  | max (x::xs)=
     if x > max xs then x
      else max xs;
max[1,2,3];

(*#8 convert of type (‘a * ‘b) list -> ‘a list * ‘b list*)
fun convert []= ([],[])
  | convert ((c,d)::xs)=
    let
      val (cs,ds)=convert xs
    in
      (c::cs, d::ds)
    end;

(*9BST makeBST of type 'a list → ('a * 'a → bool) → 'a tree*)
(*Data type*)
datatype 'data tree=
  Empty|
  Node of 'data tree * 'data * 'data tree;
(*insert*)
fun insert (x, Empty)= Node(Empty, x ,Empty)
  | insert (x, Node(l,root,r)) =
(*x < root go left*)
    if x < root then Node(insert (x,l),root,r)
(*x > root go right*)
    else if x > root then Node(insert (x,r),root,l)
(*n = root return node*)
    else Node(l,root,r);
fun makeBST [] = Empty
  | makeBST (x::xs) = insert(x, makeBST xs);
makeBST [5,2,8,1,3];

(*#10 searchBST of type ''a tree → (''a * ''a → bool) → ''a → bool *)
fun searchBST (x, Empty) = false
  | searchBST (x, Node(l,root,r))=
    x=root
    orelse searchBST (x,l)
    orelse searchBST (x,r);

searchBST (4, makeBST [5,2,8,1,3]);



