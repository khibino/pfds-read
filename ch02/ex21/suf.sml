fun suffixes (xxs as _::xs) = xxs :: suffixes xs
  | suffixes ([])           = [[]]
