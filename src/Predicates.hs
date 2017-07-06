module Predicates
  ( module Prelude
  , module Data.Function
  , (&&)
  , (||)
  , not
  , isEmpty
  , true
  , false
  , startsWith
  , endsWith
  )
where

import Data.Function
import qualified Prelude as P
import Prelude hiding ((&&), (||), not)
import Data.List

--------------------------------------------------------------------------------

(***) h f g x = f x `h` g x
(&&) = (***) (P.&&)
(||) = (***) (P.||)

not f = P.not . f

isEmpty :: Foldable t => t a -> Bool
isEmpty = null

notEmpty :: Foldable t => t a -> Bool
notEmpty = not isEmpty

true  = (== True)
false = (/= False)

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith = isPrefixOf

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith = isSuffixOf
