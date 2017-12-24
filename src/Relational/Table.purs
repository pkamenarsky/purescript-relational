module Relational.Table where

import Data.Map as M
import Data.Record as R
import Data.Symbol
import Type.Row (class RowLacks)
import Undefined (undefined)
import Prelude (($))

data Index v a = HashIndex (a -> v)

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
 { indices = R.insert index (HashIndex $ R.get index) table.indices
 }

infixr 6 addHashIndex as :-:

--------------------------------------------------------------------------------

type Person =
  { name :: String
  , age :: Int
  }

persons1 :: Table
  { name :: Index String Person
  , age  :: Index Int Person
  } Person
persons1 =
      (SProxy :: SProxy "age")
  :-: (SProxy :: SProxy "name")
  :-: new
