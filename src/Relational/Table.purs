module Relational.Table where

import Data.Symbol

import Data.Map as M
import Data.Maybe (Maybe)
import Data.Record as R
import Prelude (($), (<>))
import Type.Row (class RowLacks, Cons, Nil, kind RowList)
import Undefined (undefined)

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

class AddToIndex (r :: RowList) a where
  ai :: a -> r -> r

instance ai1 :: AddToIndex Nil a
instance ai2 :: AddToIndex as a => AddToIndex (Cons f (Index v a) as) a

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
