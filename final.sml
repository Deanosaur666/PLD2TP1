(* Q1 - Alan's Answer *)
fun min3(x, y, z) =
  if x < y then
      if x < z then x
      else z
  else if y < z then y
  else z
;

(* Q2 - Ian's Answer *)
fun cycle [] _ = []
  | cycle list 0 =  list
  | cycle (x::xs) num = cycle (xs @ [x]) (num - 1);

(* Q3 - Aty's Answer *)
fun isPrime a =
if a <= 1  then false
else if a = 2  then true
else if  a mod 2 = 0 then false
else let
      fun check d =
      if d*d > a  then true
      else if a mod d = 0 then false
      else check (d+2)
    in
      check (3)
    end;

(* Q4 - Dean's Answer *)
fun select (nil, f) = nil
  | select(a::at, f) =
  if f a then
    a::select(at, f)
  else
    select(at, f);

(* Q5 - Dean's Answer *)
fun band nil = true
  | band a = foldr (fn (x, y) => x orelse y) false a;

(* Q6 - Combined Answer *)
fun dupList [] = []
  | dupList (a::at) = a::a::dupList at;

(* Q7 - Aty's Answer *)
fun max [x] = x
  | max (x::xs) =
     if x > max xs then x
      else max xs;
