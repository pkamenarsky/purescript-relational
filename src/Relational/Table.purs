module Relational.Table where

import Data.Maybe
import Data.Symbol
import Data.Traversable
import Data.Tuple
import Type.Data.Symbol

import Data.Map as M
import Data.Record as R
import Prelude (($), (<>), flip, (<<<))
import Type.Row (class RowLacks, Cons, Nil, kind RowList)
import Undefined (undefined)

data Ref a = Ref Int

type Permissions read update create =
  { read   :: read
  , update :: update
  , create :: create
  }

data Index v a = HashIndex (a -> v) (M.Map v (Array a))

data Table indices a = Table
  { pk :: Int
  , map :: M.Map Int a
  , indices :: indices
  }

new :: ∀ a. Table {} a
new = Table
  { pk: 0
  , map: M.empty
  , indices: {}
  }

insertWith :: ∀ k v. (v -> v -> v) -> k -> v -> M.Map k v -> M.Map k v
insertWith = undefined

-- class AddToIndex (r :: RowList) a where
--   ai :: a -> r -> r
-- 
-- instance ai1 :: AddToIndex Nil a where
--   ai = undefined
-- 
-- instance ai2 :: AddToIndex as a => AddToIndex (Cons f (Index v a) as) a where
--   ai = undefined

addToIndex :: ∀ a v. a -> Index v a -> Index v a
addToIndex a (HashIndex f m) = HashIndex f (insertWith (<>) (f a) [a] m)

insert :: ∀ indices a. a -> Table indices a -> Table indices a
insert = undefined

lookup :: ∀ index indices r i a. RowCons index (Index i a) r indices => SProxy index -> i -> Table (Record indices) a -> Maybe a
lookup = undefined

addHashIndex
  :: ∀ ii io iname v r a.
     IsSymbol iname
  => RowLacks iname ii
  => RowCons iname (Index v (Record a)) ii io
  => RowCons iname v r a
  => SProxy iname
  -> Table (Record ii) (Record a)
  -> Table (Record io) (Record a)
addHashIndex index (Table table) = Table $ table
 { indices = R.insert index (HashIndex (R.get index) M.empty) table.indices
 }

infixr 6 addHashIndex as :-:

--------------------------------------------------------------------------------

type Person =
  { name :: String
  , age :: Int
  }

person :: Person
person =
  { name: "phil"
  , age: 6
  }

type PersonTable = Table
  { name :: Index String Person
  , age  :: Index Int Person
  }
  Person

persons :: PersonTable
persons =
      (SProxy :: SProxy "age")
  :-: (SProxy :: SProxy "name")
  :-: new

insert_test :: PersonTable
insert_test = insert person persons

lookup_test :: Maybe Person
lookup_test = lookup (SProxy :: SProxy "age") 5 persons

--------------------------------------------------------------------------------

data Map (o :: Symbol) k v
data Array' (o :: Symbol) a

data O (o :: Symbol)
data OLog (o :: Symbol)
data OMul (o1 :: Type) (o2 :: Type)

infixr 6 type OMul as ×

data Query (o :: Type) a

lookupO :: ∀ k v o. k -> Map o k v -> Query (OLog o) (Maybe v)
lookupO = undefined

elemsO :: ∀ k v o. Map o k v -> Query (O o) v
elemsO = undefined

mapQ :: ∀ a b o. (a -> b) -> Query o a -> Query o b
mapQ = undefined

pureQ :: ∀ a. a -> Query (O "1") a
pureQ = undefined

bindQ :: ∀ a b o1 o2. Query o1 a -> (a -> Query o2 b) -> Query (o1 × o2) a
bindQ = undefined

users :: Map "u" String String 
users = undefined

addresses :: Map "a" String String 
addresses = undefined

-- test :: Query (O "u" × O "a" × OLog "u" × O "1") String
test = do
  user    <- elemsO users
  address <- elemsO addresses
  a <- lookupO address users
  pureQ a
  where
    bind = bindQ

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

--------------------------------------------------------------------------------

class Migration a b where
  migrate :: a -> b

data A
data B
data C

{-
* idea, having this:

migrations = 
  { migration1 :: A -> B
  , migration2 :: B -> C
  , migration3 :: C -> D
  }
  
* then this should work:

d :: D
d :: migrate migrations (undefined :: A)

c :: C
c :: migrate migrations (undefined :: B)

...etc
-}
