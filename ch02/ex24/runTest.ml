#load "ins.cmo";;
open Ins.IntSet;;
let t = insert (4, insert(1, insert (2, empty)));;
dprint (insert (3, t));;
dprint (insert (4, t));;
