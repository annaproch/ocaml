Interval arithmetic

Example usage:

let x = wartosc_od_do (-2.) (-1.);;
let y = wartosc_od_do 0. 1.;;
let z = podzielic x y;;

min_wartosc z;;
- : float = neg_infinity
max_wartosc z;;
- : float = -1.


let x = wartosc_od_do (-2.) 3.;;
let y = wartosc_od_do (-4.) 5.;;
let z = razy x y;;

min_wartosc z;;
- : float = -12.
max_wartosc z;;
- : float = 15.

let a = wartosc_dokladna 1.;;
let b = wartosc_od_do (-1.) 1.;;
let c = podzielic a b;;
(* c = (-inf,-1) \cup (1,+inf) *)
let d = podzielic a c;;
(* d = (-1,1) *)

