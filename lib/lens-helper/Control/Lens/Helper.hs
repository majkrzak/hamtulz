module Control.Lens.Helper ((·), (°), mrs, mpu, maybe') where

import Control.Lens (ALens', Lens', coerced, lens, non)
import Data.Coerce (Coercible)
import Data.Empty (Empty, empty)
import Data.Text (Text, pack, unpack)

-- | Compose list of setters against value
(·) :: [a -> a] -> a -> a
(·) = foldr (flip (.)) id

infix 5 ·

-- | Compose "non empty" Lenses
(°) :: (Empty b, Eq b, Coercible b c) => Lens' a (Maybe b) -> Lens' c d -> Lens' a d
lhs ° rhs = lhs . non empty . coerced . rhs

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

-- | Lens wrapper for pack unpack operation
mpu :: Lens' (Maybe Text) (Maybe String)
mpu =
  lens
    ( \case
        Nothing -> Nothing
        Just a -> Just $ unpack a
    )
    ( \_ -> \case
        Nothing -> Nothing
        Just a -> Just $ pack a
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
