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

