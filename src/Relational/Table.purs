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

data Table indices refby a = Table
  { pk :: Int
  , map :: M.Map Int a
  , indices :: indices
  , refby :: refby
  }

new :: ∀ a. Table {} {} a
new = Table
  { pk: 0
  , map: M.empty
  , indices: {}
  , refby: {}
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

insert :: ∀ indices refby a. a -> Table indices refby a -> Table indices refby a
insert = undefined

lookup :: ∀ index indices refby r i a. RowCons index (Index i a) r indices => SProxy index -> i -> Table (Record indices) refby a -> Maybe a
lookup = undefined

addHashIndex
  :: ∀ ii io iname v r a refby.
     IsSymbol iname
  => RowLacks iname ii
  => RowCons iname (Index v (Record a)) ii io
  => RowCons iname v r a
  => SProxy iname
  -> Table (Record ii) refby (Record a)
  -> Table (Record io) refby (Record a)
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
  {}
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
