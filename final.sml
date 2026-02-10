(* Q1 - Alan's Answer *)
fun min3(x, y, z) =
  if x < y then
      if x < z then x
      else z
  else if y < z then y
  else z
;

min3(1, 2, 2);

(* Q2 - Ian's Answer *)
fun cycle [] _ = []
  | cycle list 0 =  list
  | cycle (x::xs) num = cycle (xs @ [x]) (num - 1);

cycle [1, 2, 3, 4] 3;

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

isPrime 13;
isPrime ~13;
isPrime 7583;
isPrime 7585;

(* Q4 - Dean's Answer *)
fun select (nil, f) = nil
  | select(a::at, f) =
  if f a then
    a::select(at, f)
  else
    select(at, f);

select ([1, 3, 12, 13, ~13, 7583, 7585], isPrime);

(* Q5 - Dean's Answer *)
fun band nil = true
  | band a = foldr (fn (x, y) => x orelse y) false a;

band [];
band [true, false, false];
band [false, false];

(* Q6 - Combined Answer *)
fun dupList [] = []
  | dupList (a::at) = a::a::dupList at;

dupList [1, 2, 3, 4, 5];

(* Q7 - Aty's Answer *)
fun max [x] = x
  | max (x::xs) =
     if x > max xs then x
      else max xs;

max [~13, 7583, 13, 7585, 3];

(* Q8 - Alan and Aty's *)
fun convert [] = ([],[])
  | convert((x,y)::rest) =
    let
      val (xrest, yrest) = convert(rest)
    in
      (x::xrest, y::yrest)
    end;

convert [(1,2), (3,4), (5,6)];
