module TestO where

import O

-- test :: Query (O "u" × O "a" × OLog "u" × O "1") String
test = do
  user    <- elems users
  address <- elems addresses
  a <- lookup address users
  pure a

