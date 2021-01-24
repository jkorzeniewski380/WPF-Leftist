(* ======= Autor: Jakub Korzeniewski ======= *)
(* ============ Drzewa Lewicowe ============ *)
(* ======= Code Review: Jakub Podolak ====== *)

(* Wyjątek podnoszony gdy kolejka jest pusta *)
exception Empty;;

(* Kolejka priorytetowa w postaci drzewa 
   Null -> Kolejka jest pusta 
   Node -> Kolejka jest niepusta, argumenty:
    wartość ('a); 
    prawa wysokość (int); 
    lewe poddrzewo ('a queue);
    prawe poddrzewo ('a queue); *)
type 'a queue = Null | Node of 'a * int * 'a queue * 'a queue;;

(* Pusta kolejka priorytetowa *)
let empty = Null;;

(* Funkcja sprawdzająca czy kolejka priorytetowa jest pusta
   'a queue -> bool *)
let is_empty q = (q = Null);;

(* Funkcja zwracająca złączenie kolejek d1 i d2
   'a queue -> 'a queue -> 'a queue *)
let rec join d1 d2 =
  match d1, d2 with
    Null, d2 -> d2 |    (* Jeśli któraś kolejka jest pusta, zwracamy drugą *)
    d1, Null -> d1 |
    Node (val1, rh1, l1, r1), Node (val2, rh2, l2, r2) ->
    (* Jeśli pierwsza kolejka ma większy element w korzeniu, zamieniamy je *)
    if val1 > val2 then join d2 d1
    else 
      (* d3 -> złączenie prawego poddrzewa kolejki d1 i kolejki d2 *)
      let d3 = join r1 d2 in
      (* Ustalamy która z kolejek (l1, d3) będzie prawym poddrzewem wyniku
         Jeśli dowolna z nich jest pusta, prawym poddrzewem będzie kolejka pusta
         a lewym druga z nich
         W innym przypadku prawym poddrzewem będzie kolejka o mniejszej
         prawej wysokości *)
      match l1, d3 with
        Null, d3 -> Node (val1, 1, d3, Null) |
        l1, Null -> Node (val1, 1, l1, Null) |
        Node (val_l1, rh_l1, l_l1, r_l1), Node (val_d3, rh_d3, l_d3, r_d3) ->
        if rh_l1 < rh_d3 then Node (val1, rh_l1 + 1, d3, l1) 
        else Node(val1, rh_d3 + 1, l1, d3);;

(* Funkcja zwracająca dla niepustej kolejki d parę (e, d')
   e  -> minimalny element kolejki d
   d' -> kolejka d bez elementu e 
   'a queue -> ('a, 'a queue) *)
let delete_min d =
  match d with
    Null -> raise Empty |     (* Jeśli kolejka jest pusta podnosimy wyjątek *)
    Node (v1, rh1, l1, r1) -> (v1, join l1 r1);;

(* Funkcja dołączająca element a do kolejki d 
   'a queue -> 'a queue -> a' queue *)
let add a d = join (Node (a, 1, Null, Null)) d;; 