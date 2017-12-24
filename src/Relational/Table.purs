module Relational.Table where

import Data.Symbol
import Type.Row
import Undefined
import Prelude

data Map k v

data Index v a = HashIndex (a -> v)

data Table (indices :: # Type) a = Table a (Record indices)

type Person =
  { name :: String
  , age :: Int
  }

new :: ∀ a. Table () a
new = undefined

addHashIndex
  :: ∀ ii io iname v a.
     RowCons iname (Index v a) ii io
  => SProxy iname
  -> (a -> v)
  -> Table ii a
  -> Table io a
addHashIndex = undefined

persons :: Table () Person
persons = new

persons1 :: Table
  ( name :: Index String Person
  , age  :: Index Int Person
  )
  Person
persons1 = addHashIndex (SProxy :: SProxy "age") (_.age) $ addHashIndex (SProxy :: SProxy "name") (_.name) persons
