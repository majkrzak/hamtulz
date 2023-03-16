module Control.Lens.Helper ((·), (°), mrs, maybe') where

import Control.Lens (ALens', Lens', lens, non)
import Data.Empty (Empty, empty)

-- | Compose list of setters against value
(·) :: [a -> a] -> a -> a
(·) = foldl (.) id

infix 5 ·

-- | Compose "non empty" Lenses
(°) :: (Empty b, Eq b) => Lens' a (Maybe b) -> Lens' b c -> Lens' a c
lhs ° rhs = lhs . non empty . rhs

infixr 9 °

-- | Apply pair of methods against Lens
(®) :: Lens' a b -> (b -> c, c -> b) -> Lens' a c
lhs ® (rhs, rhs') = lhs . lens rhs (const rhs')

infixr 9 ®

-- | Lens wrapper for read show operation
mrs :: (Read a, Show a) => Lens' (Maybe a) (Maybe String)
mrs =
  lens
    ( \case
        Nothing -> Nothing
        Just a -> Just $ show a
    )
    ( \_ -> \case
        Nothing -> Nothing
        Just a -> Just $ read a
    )

-- | Wraps maybe lens
maybe' :: Lens' a (Maybe a)
maybe' =
  lens
    Just
    ( \a -> \case
        Nothing -> a
        Just b -> b
    )
