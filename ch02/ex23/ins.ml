module type SET =
sig
  type elem
  type set

  val empty  : set
  val insert : elem * set -> set
  val member : elem * set -> bool

  val dprint : set -> unit
end

module type ORDERED =
sig
  type t

  val eq  : t * t -> bool
  val lt  : t * t -> bool
  val leq : t * t -> bool

  val dprint : t -> unit
end

module UnbalancedSet (Element : ORDERED) : SET
  with type elem = Element.t =
struct
  type elem = Element.t
  type tree = E | T of tree * elem * tree
  type set  = tree

  let empty  = E

  let member =
    let rec member cand = function
    | x, T(a, y, b) ->
      if Element.lt (x, y) then member cand (x, a)
      (* replace candidate *)
      else member (Some y) (x, b)
    | x, E          -> begin match cand with
        | Some cv when Element.eq (cv, x) -> true
        | _                               -> false
    end
    in member None

  let rec insert' = function
    | x, (T (a, y, b) as s) ->
      if Element.lt (x, y) then T (insert' (x, a), y, b)
      else if Element.lt (y, x) then T(a, y, insert' (x, b))
      else s
    | x, E                  -> T (E, x, E)

  let insert'' (x, tree) =
    let rec insert x =
      function
        | E                           -> Some (T (E, x, E))
        | T (a, y, b) when Element.lt (x, y) -> begin match insert x a with
            | Some ins -> Some (T (ins, y, b))
            | None     -> None
          end
        | T (a, y, b) when Element.lt (y, x) -> begin match insert x b with
            | Some ins -> Some (T (a, y, ins))
            | None     -> None
          end
        | T (a, y, b) -> None
    in
      match insert x tree with
        | Some ins -> ins
        | None     -> tree

  exception Already_exist of elem

  let insert (x, tree) =
    let rec insert x =
      function
        | E                           -> T (E, x, E)
        | T (a, y, b) when Element.lt (x, y) ->
          T (insert x a, y, b)
        | T (a, y, b) when Element.lt (y, x) ->
          T (a, y, insert x b)
        | T (a, y, b) -> raise (Already_exist x)
    in
    try insert x tree with
      | Already_exist _ -> tree

  let dprint (tree : set) : unit =
    let rec dprint = function
      | E -> print_string "E"
      | T (a, y, b) ->
        print_string "T ("; dprint a;
        print_string ", " ; Element.dprint y;
        print_string ", " ; dprint b;
        print_string ")"
    in dprint tree; print_newline ();

end

module Int : ORDERED with type t = int =
struct
  type t = int

  let eq  (x,y) = x == y
  let lt  (x,y) = x <  y
  let leq (x,y) = x <= y

  let dprint = print_int
end

module IntSet = UnbalancedSet (Int)
