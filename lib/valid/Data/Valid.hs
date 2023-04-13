module Data.Valid (Valid, Validator, valid, validate, validator, mkValidator, mkValidatorComment, mkRecursiveValidator, mkListRecursiveValidator, mkMaybeRecursiveValidator, mkMaybeValidator, mkDescriptor, mkNested, mkLabel) where

import Data.Coerce (coerce)
import Data.Maybe (maybeToList)

newtype Validator a = Validator (a -> [String])

instance Semigroup (Validator a) where
  Validator lhs <> Validator rhs = Validator $ \a -> lhs a <> rhs a

instance Monoid (Validator a) where
  mempty = Validator $ const []

validate :: Validator a -> a -> [String]
validate = coerce

class Valid a where
  validator :: Validator a

valid :: Valid a => a -> Bool
valid = null . validate validator

mkValidator :: String -> (a -> Bool) -> Validator a
mkValidator name rule = Validator $ \a -> [name | rule a]

mkValidatorComment :: (a -> Maybe String) -> Validator a
mkValidatorComment rule = Validator $ \a -> maybeToList $ rule a

mkLabel :: (a -> String) -> Validator a -> Validator a
mkLabel p v = Validator $ \a -> [p a <> ":" <> n | n <- validate v a]

mkNested :: (a -> b) -> Validator b -> Validator a
mkNested n (Validator b) = Validator $ \a -> b (n a)

mkMaybeValidator :: String -> (a -> Bool) -> Validator (Maybe a)
mkMaybeValidator name rule = Validator $ maybe [] (\a -> [name | rule a])

mkDescriptor :: (a -> String) -> [Validator a] -> Validator a
mkDescriptor descriptor vs = Validator $ \a -> [descriptor a <> ":" <> name | Validator v <- vs, name <- v a]

mkRecursiveValidator :: Valid b => String -> (a -> b) -> Validator a
mkRecursiveValidator prefix a2b = Validator $ \a -> [prefix <> ":" <> name | name <- validate validator (a2b a)]

mkListRecursiveValidator :: Valid b => String -> (a -> [b]) -> Validator a
mkListRecursiveValidator prefix a2lb = Validator $ \a -> [prefix <> ":" <> name | names <- a2lb a, name <- validate validator names]

mkMaybeRecursiveValidator :: Valid b => String -> (a -> Maybe b) -> Validator a
mkMaybeRecursiveValidator prefix a2mb = Validator $ \a -> [prefix <> ":" <> name | name <- maybe [] (validate validator) (a2mb a)]
