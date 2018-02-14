module O where

import Data.Maybe
import Undefined (undefined)

data Map (o :: Symbol) k v
data Array' (o :: Symbol) a

data O (o :: Symbol)
data OLog (o :: Symbol)
data OMul (o1 :: Type) (o2 :: Type)

infixr 6 type OMul as ×

data Query (o :: Type) a

lookup :: ∀ k v o. k -> Map o k v -> Query (OLog o) (Maybe v)
lookup = undefined

elems :: ∀ k v o. Map o k v -> Query (O o) v
elems = undefined

map :: ∀ a b o. (a -> b) -> Query o a -> Query o b
map = undefined

pure :: ∀ a. a -> Query (O "1") a
pure = undefined

bind :: ∀ a b o1 o2. Query o1 a -> (a -> Query o2 b) -> Query (o1 × o2) a
bind = undefined

users :: Map "u" String String 
users = undefined

addresses :: Map "a" String String 
addresses = undefined

{-
simplify :: ∀ i o a. Simplify i o => Query i a -> Query o a
simplify = undefined

class Simplify i o | i -> o

instance simplifyOa_1 :: IsSymbol o => Simplify (O o) (O o)

instance simplifyOa_2 :: IsSymbol o => Simplify (OLog o) (OLog o)

instance simplifyOb_1 :: Simplify a b => Simplify (a × O "1") b

instance simplifyOb_2 :: Simplify a b => Simplify (O "1" × a) b

instance simplifyOmul_1 :: (IsSymbol a, IsSymbol b) => Simplify (O a × O b) (O a × O b)
instance simplifyOmul_2 :: (IsSymbol a, IsSymbol b) => Simplify (O a × OLog b) (O a × OLog b)
instance simplifyOmul_3 :: (IsSymbol a, IsSymbol b) => Simplify (OLog a × OLog b) (OLog a × OLog b)
instance simplifyOmul_4 :: (IsSymbol a, IsSymbol b) => Simplify (OLog a × O b) (OLog a × O b)

instance simplifyOmul_z :: (Simplify a b, Simplify c d, Simplify (b × d) e) => Simplify (a × c) e
-}
