type point = float * float;;
type kartka = point -> int;;

(* prosta p1 p2 zwraca (A, B, C), gdzie Ax + By + C = 0 to prosta przechodząca przez punkty p1 i p2, zakladamy p1 =/= p2 *)
let prosta (x1, y1) (x2, y2) = (y1 -. y2, x2 -. x1, x1 *. y2 -. y1 *. x2);;

(*odl_kw p1 p2 zwraca kwadrat odległości między punktami p1 i p2 *)
let odl_kw (x1, y1) (x2, y2) = (x1 -. x2) *. (x1 -. x2) +. (y1 -. y2)*.(y1 -. y2);;

(* sym p1 p2 p zwraca punkt symetryczny do p względem prostej przechodzącej przez p1 i p2 *)
(* używam do tego rownania prostej przechadzacej przez p1 i p2, zakladamy p1 =/= p2 *)
let sym (x1, y1) (x2, y2) = fun (x, y) ->
    let (a, b, c) = prosta (x1, y1) (x2, y2)
    in (((b *. b -. a *. a) *. x -. 2. *. a *. b *. y -. 2. *. a *. c) /. (a *. a +. b *. b), 
       (((a *. a -. b *. b) *. y -. 2. *. a *. b *. x -. 2. *. b *. c) /. (a *. a +. b *. b))
;;



(* strona p1 p2 p zwraca wartość ujemną, jeżeli p jest po lewej stronie prostej p1p2, dodatnią,
jeżeli po prawej stronie i 0 jeżeli leży na prostej (patrzymy z punktu p1 do p2) *)
let strona (x1, y1) (x2, y2) = fun (x, y) ->
  (y -. y1) *. (x2 -. x1) -. (y2 -. y1) *. (x -. x1)
;;

(* prostokat p1 p2 p zwraca 1 jezeli p zawiera sie w prostakacie o 
lewym dolnym rogu p1 i prawym gornym p2 i 0 w przeciwnym wypadku *)
let prostokat (xl, yl) (xp, yp) = fun (x, y) ->
    if  xl <= x && x <= xp && yl <= y && y <= yp then 1 else 0;;

(* kolko p r q zwraca 1 jezeli q zawiera sie w kole o promieniu r i srodku p
i 0 w przeciwnym wypadku *)
let kolko p r = fun q ->
    if odl_kw p q <= r *. r then 1 else 0;;

(*zloz p1 p2 krtk - zwraca kartkę krtk zgieta wzdloz prostej p1p2, ze strony lewej na prawa (patrzac z p1 na p2), zakladamy p1 =/= p2 *)
let zloz p1 p2 krtk = fun p ->
    let str = strona p1 p2 p
    in
    if str > 0. then (krtk p) + (krtk (sym p1 p2 p))
    else if str < 0. then 0
         else krtk p
;;
(*skladaj l krtk - sklada kartke krtk zdloz prostych zdefiniowanych przez pary punktow
zawartch w liscie l, zakladamy, ze zadna para nie zawiera dwoch takich samych punktow*)
let skladaj l krtk = List.fold_left (fun a (p1, p2) -> zloz p1 p2 a) krtk l;;
