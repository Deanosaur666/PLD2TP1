(* power *)
fun power (a : real, 0) = 1.0
  | power (a : real, b : int) =
  if b < 0 then 1.0/(power(a, ~b))
  else if(b mod 2 = 0) then power(a*a, b div 2)
  else a * power (a, b-1);

power(3.0, 2);
power(2.0, ~2);
power(2.0, 4);

(* 1 *)
fun min3 (a, b, c) = 
if a < b andalso a < c then a
else if b < a andalso b < c then b
else c;

(* 2 *)
fun cycle (list, 0) = list
  | cycle (list, i) = cycle(tl list @ [hd list], i - 1);

cycle([1,2,3,4,5,6], 2);

(* 3 *)

fun isPrime 0 = false
  | isPrime 1 = false
  | isPrime a = 
  let
    fun divisable(a, b) =
      if b < 2 then true
      else if a mod b = 0 then false
      else divisable(a, b-1)
  in
    divisable(a, a-1)
  end;

isPrime 11;
