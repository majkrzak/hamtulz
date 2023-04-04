module Data.Valid (Valid, Validator, valid, validate, validator, mkGroup, mkValidator, mkRecursiveValidator, mkListRecursiveValidator, mkMaybeRecursiveValidator, mkMaybeValidator, mkDescriptor) where

newtype Validator a = Validator (a -> [String])

class Valid a where
  validator :: Validator a

validate :: Valid a => a -> [String]
validate = validator'
  where
    Validator validator' = validator

valid :: Valid a => a -> Bool
valid = null . validate

mkValidator :: String -> (a -> Bool) -> Validator a
mkValidator name rule = Validator $ \a -> [name | rule a]

mkMaybeValidator :: String -> (a -> Bool) -> Validator (Maybe a)
mkMaybeValidator name rule = Validator $ maybe [] (\a -> [name | rule a])

mkGroup :: (a -> String) -> (a -> b) -> [Validator b] -> Validator a
mkGroup prefix a2b vs = Validator $ \a -> [prefix a <> ":" <> name | Validator v <- vs, name <- v (a2b a)]

mkDescriptor :: (a -> String) -> [Validator a] -> Validator a
mkDescriptor descriptor vs = Validator $ \a -> [descriptor a <> ":" <> name | Validator v <- vs, name <- v a]

mkRecursiveValidator :: Valid b => String -> (a -> b) -> Validator a
mkRecursiveValidator prefix a2b = Validator $ \a -> [prefix <> ":" <> name | name <- validate (a2b a)]

mkListRecursiveValidator :: Valid b => String -> (a -> [b]) -> Validator a
mkListRecursiveValidator prefix a2lb = Validator $ \a -> [prefix <> ":" <> name | names <- a2lb a, name <- validate names]

mkMaybeRecursiveValidator :: Valid b => String -> (a -> Maybe b) -> Validator a
mkMaybeRecursiveValidator prefix a2mb = Validator $ \a -> [prefix <> ":" <> name | name <- maybe [] validate (a2mb a)]
