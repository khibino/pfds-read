
let rec suffixes = function
  | (_::xs) as xxs -> xxs :: suffixes xs
  | []             -> [[]]
