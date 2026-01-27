(* Dean Yockey *)

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
  if a < 0 then false
  else
  let
    fun divisable(a, b) =
      if b < 2 then true
      else if a mod b = 0 then false
      else divisable(a, b-1)
  in
    divisable(a, a-1)
  end;

isPrime 11;
isPrime ~11;

(* 4 *)
fun select (nil, f) = nil
  | select(a::at, f) = 
  if f a then
    a::select(at, f)
  else
    select(at, f);

select([1,2,3,4,5,6,7,8,9,10], isPrime);

(* 5 *)
(* TODO: use mapreduce *)
fun band (nil) = true
  | band (a::at) = a orelse (not (null at) andalso band(at));

band(nil);
band([false, false]);

(* 6 *)
fun duplist (nil) = nil
  | duplist (a::at) = a::a::duplist(at);

duplist [1, 3, 2];

(* 7 *)
fun max (a::at) =
  if null at then a
  else if a > max (at) then a
  else max(at);

max([1, 4, ~3, 2]);

(* 8 *)
(* convert list of pairs (tuples) to a pair of lists (a tuple of two lists) *)
(* this is horrible to look at *)
fun convert (nil) = (nil, nil)
  | convert (lop : ('a * 'b) list) : ('a list * 'b list) =
  let
    fun addfirst(nil, lista) = lista
        | addfirst(lop::lopt : ('a * 'b) list, lista) = (#1 lop)::addfirst(lopt, lista)
    fun addsecond(nil, listb) = listb
        | addsecond(lop::lopt : ('a * 'b) list, listb) = (#2 lop)::addsecond(lopt, listb)
    in
      let
        fun addpair(lop, (pola, polb)) = (addfirst(lop, pola), addsecond(lop, polb))
    in
      addpair(lop, (nil, nil))
end
end;

convert [(1,2), (3,4), (5,6)];

(* in class *)
fun factorial n =
  case n of
    0 => 1
  | _ => n * factorial (n - 1);

fun odd_or_even n =
  case n mod 2 of
    0 => "even"
  | 1 => "odd";

odd_or_even 1;

fun dibide a = fn b => a div b;
dibide 6 2;

fun triple a b c = [a, b, c];
triple 2 3 4;

val double = triple 0;
double 2 3;

fun beb nil a = [a]
  | beb [x] a = a :: [x]
  | beb (x::xt) a = a :: [hd xt];

beb [1] 2;
beb [1, 2, 3, 4] 7;
beb nil 3;

foldl (op ::) nil [1, 2, 3, 4, 5];
