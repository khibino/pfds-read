
exception NotFound

module type FINITEMAP =
sig
  type key
  type 'a map 

  val empty  : 'a map
  val bind   : key * 'a * 'a map -> 'a map
  val lookup : key * 'a map -> 'a

  val dprint : 'a map -> unit
end

module type ORDERED =
sig
  type t

  val eq  : t * t -> bool
  val lt  : t * t -> bool
  val leq : t * t -> bool

  val dprint : t -> unit
end

module UnbalancedMap (Key : ORDERED) : FINITEMAP
  with type key = Key.t =
struct
  type key = Key.t
  type 'a tree = E | T of 'a tree * 'a * 'a tree
  type 'a map  = (key * 'a) tree

  let empty  = E

  let lookup (xk, tree) =
    let rec lookup cand = function
    | T(a, (yk, _ as y), b) ->
      if Key.lt (xk, yk) then lookup cand a
      else lookup (Some y) b  (* replace candidate *)
    | E                     -> begin match cand with
        | Some (ck, cv) when Key.eq (ck, xk) -> cv
        | _                                  -> raise NotFound
    end
    in
    lookup None tree

  let bind (xk, xv, tree) =
    let rec bind = function
      | T (a, (yk, _ as y), b) ->
        if Key.lt (xk, yk) then T (bind a, y, b)
        else if Key.lt (yk, xk) then T(a, y, bind b)
        else T (a, (xk, xv), b)
      | E                      -> T (E, (xk, xv), E)
    in
    bind tree

  let dprint (tree : 'a map) : unit =
    let rec dprint = function
      | E -> print_string "E"
      | T (a, (yk, yv), b) ->
        print_string "T ("; dprint a;
        print_string ", ("; Key.dprint yk;
        print_string ", " ; print_string (Std.dump yv);
        print_string "), "; dprint b;
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

module IntMap = UnbalancedMap (Int)
