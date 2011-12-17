module type ORDERED =
sig
  type t

  val eq  : t * t -> bool
  val lt  : t * t -> bool
  val leq : t * t -> bool

  val dprint : t -> unit
end

module type HEAP =
sig
  module Elem : ORDERED

  type heap

  val empty   : heap
  val isEmpty : heap -> bool
  val insert  : Elem.t * heap -> heap
  val merge   : heap * heap -> heap

  val findMin   : heap -> Elem.t
  val deleteMin : heap -> heap

  val dprint : heap -> unit
end

module LeftistHeap (Element : ORDERED) : HEAP with module Elem = Element =
struct
  module Elem = Element

  type heap = E | T of int * Elem.t * heap * heap

  let rank = function
    | E              -> 0
    | T (r, _, _, _) -> r

  let makeT (x, a, b) =
    if rank a >= rank b then T (rank b + 1, x, a, b)
    else T (rank a + 1, x, b, a)

  let empty = E
  let isEmpty = function | E -> true | _ -> false

  let rec merge = function
    | (h, E) -> h
    | (E, h) -> h
    | ( (T (_, x, a_1, b_1) as h_1), (T (_, y, a_2, b_2) as h_2) ) ->
        if Elem.leq (x, y) then makeT (x, a_1, merge (b_1, h_2))
        else makeT (y, a_2, merge (h_1, b_2))

  let insert' (x, h) = merge (T (1, x, E, E), h)

  let rec insert = function
    | (x, E)                           -> T(1, x, E, E)
    | (x, (T (_, y, a_2, b_2) as h_2)) ->
      if Elem.leq (x, y) then T (1, x, h_2, E) (* makeT (x, E, merge (E, h_2)) *) (* makeT (x, E, h_2) *)
      else makeT (y, a_2, insert (x, b_2))

  exception Empty of string
  let emptyWith s = raise (Empty s)

  let findMin   = function | E -> emptyWith "findMin" | T (_, x, a, b) -> x
  let deleteMin = function
    | E -> emptyWith "deleteMin"
    | T (_, x, a, b) -> merge (a, b)

  let dprint heap =
    let rec dprint = function
      | E -> print_string "E"
      | T (r, y, a, b) ->
        print_string "T ("; print_int r;
        print_string ", " ; Element.dprint y;
        print_string ", " ; dprint a;
        print_string ", " ; dprint b;
        print_string ")"
    in dprint heap; print_newline ();
end

module Int : ORDERED with type t = int =
struct
  type t = int

  let eq  (x,y) = x == y
  let lt  (x,y) = x <  y
  let leq (x,y) = x <= y

  let dprint = print_int
end

module IntHeap = LeftistHeap (Int)
