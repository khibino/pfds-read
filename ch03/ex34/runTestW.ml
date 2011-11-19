#load "wHeap.cmo";;
open WHeap.IntHeap;;
let t = insert (5, insert(1, insert (2, empty)));;
dprint (insert (3, t));;
dprint (insert (4, (insert (3, t))));;

