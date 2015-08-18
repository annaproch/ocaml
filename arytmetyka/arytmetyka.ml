(* Niezmiennik typu: pocz <= kon
 * Jeśli przedział jest postaci <pocz; kon> to czy_dopeln = false
 * a jeśli przedział jest postaci (-oo; pocz> v <kon; oo) to czy_dopeln = true *)
type wartosc = {pocz: float; kon: float; czy_dopeln: bool};;

(* KONSTRUKTORY *)
let wartosc_dokladnosc x p = {
    pocz = x -. (x *. p /. 100.);
    kon = x +. (x *. p /. 100.);
    czy_dopeln = false
};;

let wartosc_od_do x y = {
    pocz = x;
    kon = y;
    czy_dopeln = false
};;

let wartosc_dokladna x  = {
    pocz = x;
    kon = x;
    czy_dopeln = false
};;

let wartosc_dopeln x y = {
    pocz = x;
    kon = y;
    czy_dopeln = true
};;

(* SELEKTORY *)
let in_wartosc w x =
    match w.czy_dopeln with
    |false -> if (x <= w.kon && x >= w.pocz) then true else false
    |true -> if (x >= w.kon || x <= w.pocz) then true else false;;

let min_wartosc w =
    match w.czy_dopeln with
    |false -> w.pocz
    |true -> neg_infinity;;

let max_wartosc w =
    match w.czy_dopeln with
    |false -> w.kon
    |true -> infinity;;

let sr_wartosc w =
    let mini, maxi = min_wartosc w, max_wartosc w in
    if (mini = neg_infinity || maxi = infinity) then nan
        else (mini +. maxi) /. 2.;;

(* MODYFIKATORY I PROCEDURY POMOCNICZE *)
let rzeczywiste = wartosc_od_do neg_infinity infinity;;

(* Zwraca wartość, dla której niezmiennik (pocz <= kon) jest zachowany *)
let valid w =
    if (w.czy_dopeln = true && w.pocz > w.kon) then rzeczywiste
        else w;;

(* Jeśli dla argumentow zachodzi czy_dopeln = false to dla wynikowej wartosci niezmiennik jest zachowany,
 * ponieważ dla argumentów jest zachowany;
 * w przeciwnym przypadku walidujemy wartości, aby były poprawne *)
let plus w1 w2 =
    match w1.czy_dopeln, w2.czy_dopeln with
    |false, false -> wartosc_od_do (w1.pocz +. w2.pocz) (w1.kon +. w2.kon)
    |false, true -> valid (wartosc_dopeln (w1.kon +. w2.pocz) (w1.pocz +. w2.kon))
    |true, false -> valid (wartosc_dopeln (w2.kon +. w1.pocz) (w2.pocz +. w1.kon))
    |true, true -> rzeczywiste;;

(* Dowód poprawności analogiczny jak dla procedury plus *)
let minus w1 w2 =
    match w1.czy_dopeln, w2.czy_dopeln with
    |false, false -> wartosc_od_do (w1.pocz -. w2.kon) (w1.kon -. w2.pocz)
    |false, true -> valid (wartosc_dopeln (w1.kon -. w2.kon) (w1.pocz -. w2.pocz))
    |true, false -> valid (wartosc_dopeln (w1.pocz -. w2.pocz) (w1.kon -. w2.kon))
    |true, true -> rzeczywiste;;

(* Zwraca wartość ilorazu 1. / wartosc *)
let odwrotnosc w =
    if (w = rzeczywiste || w.pocz = neg_infinity && w.kon = 0. ||
        w.pocz = 0. && w.kon = infinity) then w
    else if w.czy_dopeln = true then wartosc_od_do (1. /. w.pocz) (1. /. w.kon)
    else if w.pocz = 0. then wartosc_od_do (1. /. w.kon) infinity
    else if w.kon = 0. then wartosc_od_do neg_infinity (1. /. w.pocz)
    else if (w.pocz < 0. && w.kon > 0.) || (w.pocz = neg_infinity && w.kon > 0.) || 
        (w.pocz < 0. && w.kon = infinity) then wartosc_dopeln (1. /. w.pocz) (1. /. w.kon)
    else wartosc_od_do (1. /. w.kon) (1. /. w.pocz);;

let is_nan x = not (x = x);;

(* Jeśli dwie liczby to nan to zwraca nan, w przeciwnym przypadku zwraca mniejszą liczbę
 * różną od nan *)
let mini a b =
    if is_nan a then b else if is_nan b then a
        else if a <= b then a else  b;;

(* Jeśli dwie liczby to nan to zwraca nan, w przeciwnym przypadku zwraca większą liczbę
 * różną od nan *)
let maxi a b =
    if is_nan a then b else if is_nan b then a
        else if a <= b then b else a;;

(* Mnożenie przedziałów, dla których czy_dopeln = false;
 * obliczając minimum i maximum, jeśli możemy to zwracamy wartości określone
 * i pomijamy nan *)
let razy_pom w1 w2 =
    let pocz = mini (w1.pocz *. w2.pocz) 
        (mini (w1.pocz *. w2.kon) (mini (w1.kon *. w2.pocz) (w1.kon *. w2.kon)))
    and kon = maxi (w1.pocz *. w2.pocz) 
        (maxi (w1.pocz *. w2.kon) (maxi (w1.kon *. w2.pocz) (w1.kon *. w2.kon)))
    in wartosc_od_do pocz kon;;

(* Zwraca sumę teoriomnogościową przedziałów *)
let suma_przed w1 w2 =
    if (w1.pocz >= w2.pocz && w1.kon <= w2.kon) then w2
        else if (w1.pocz <= w2.pocz && w1.kon >= w2.kon) then w1
            else if (w1.pocz <= w2.kon && w1.kon >= w2.pocz) then wartosc_od_do w2.pocz w1.kon
                else if (w2.pocz <= w1.kon && w2.kon >= w1.pocz) then wartosc_od_do w1.pocz w2.kon
                    else wartosc_dopeln (min w1.kon w2.kon) (max w1.pocz w2.pocz);;

(* Mamy określone mnożenie wartości, dla ktorych czy_dopeln = false;
 * jeśli jedna z wartości ma czy_dopeln = true to rozbijamy ją na sume przedziałów, dla
 * których mamy określone mnożenie i zwracamy sumę mnogościową wyniku;
 * dla odwrotności wartości z czy_dopeln = true zachodzi czy_dopeln = false,
 * więc dobrze okreslamy iloczyn dwoch wartości z czy_dopeln = true *)
let razy w1 w2 =
    match w1.czy_dopeln, w2.czy_dopeln with
    |false, false -> razy_pom w1 w2
    |true, false -> suma_przed (razy_pom (wartosc_od_do neg_infinity w1.pocz) w2) 
                               (razy_pom (wartosc_od_do w1.kon infinity) w2)
    |false, true -> suma_przed (razy_pom (wartosc_od_do neg_infinity w2.pocz) w1) 
                               (razy_pom (wartosc_od_do w2.kon infinity) w1)
    |true, true -> odwrotnosc (razy_pom (odwrotnosc w1) (odwrotnosc w2));;

let podzielic w1 w2 =
    razy w1 (odwrotnosc w2);
