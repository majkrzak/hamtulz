module Control.Lens.Helper ((°)) where

import Control.Lens (ALens', Lens', non)
import Data.Empty (Empty, empty)

-- | Compose list of setters against value
(·) :: [a -> a] -> a -> a
(·) = foldl (.) id

infix 4 ·

-- | Compose "non empty" Lenses
(°) :: (Empty b, Eq b) => Lens' a (Maybe b) -> Lens' b c -> Lens' a c
lhs ° rhs = lhs . non empty . rhs

infixr 9 °
