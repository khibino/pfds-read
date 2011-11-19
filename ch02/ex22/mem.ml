module type SET =
sig
  type elem
  type set

  val empty  : set
  val insert : elem * set -> set
  val member : elem * set -> bool
end

module type ORDERED =
sig
  type t

  val eq  : t * t -> bool
  val lt  : t * t -> bool
  val leq : t * t -> bool
end

module UnbalancedSet (Element : ORDERED) : SET =
struct
  type elem = Element.t
  type tree = E | T of tree * elem * tree
  type set  = tree

  let empty  = E

  let rec member' = function
    | x, T(a, y, b) ->
      if Element.lt (x,y) then member' (x,a)
      else if Element.lt (y,x) then member' (x,b)
      else true
    | x, E          -> false

  let member (x, tree) =
    let rec member cand x = function
    | T(a, y, b) ->
      if Element.lt (x, y) then member cand x a
      (* replace candidate *)
      else member (Some y) x b
    | E          -> begin match cand with
        | Some cv when Element.eq (cv, x) -> true
        | _                               -> false
    end
    in member None x tree

  let rec insert = function
    | x, (T (a, y, b) as s) ->
      if Element.lt (x, y) then T (insert (x, a), y, b)
      else if Element.lt (y, x) then T(a, y, insert (x, b))
      else s
    | x, E                  -> T (E, x, E)
end
