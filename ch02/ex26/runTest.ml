#load "extLib.cma";;
#load "finiteMap.cmo";;
open FiniteMap.IntMap;;
let t = bind (4, "d", bind(1, "a", bind (2, "b", empty)));;
dprint (bind (3, "c", t));;
dprint (bind (4, "e", t));;
