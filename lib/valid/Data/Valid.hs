module Data.Valid (Valid, Validator, valid, validate, validators, mkGroup, mkValidator, mkRecursiveValidator, mkListRecursiveValidator, mkMaybeRecursiveValidator) where

newtype Validator a = Validator (a -> [String])

class Valid a where
  validators :: [Validator a]

validate :: Valid a => a -> [String]
validate a = [v' | Validator v <- validators, v' <- v a]

valid :: Valid a => a -> Bool
valid = null . validate

mkValidator :: String -> (a -> Bool) -> Validator a
mkValidator name rule = Validator $ \a -> [name | rule a]

mkGroup :: String -> (a -> b) -> [Validator b] -> Validator a
mkGroup prefix a2b vs = Validator $ \a -> [ prefix <> ":" <> name | Validator v <- vs ,name <- v (a2b a)]

mkRecursiveValidator :: Valid b => String -> (a -> b) -> Validator a
mkRecursiveValidator prefix a2b = Validator $ \a -> [prefix <> ":" <> name | name <- validate (a2b a)]

mkListRecursiveValidator :: Valid b => String -> (a -> [b]) -> Validator a
mkListRecursiveValidator prefix a2lb = Validator $ \a -> [prefix <> ":" <> name | names <- a2lb a, name <- validate names]

mkMaybeRecursiveValidator :: Valid b => String -> (a -> Maybe b) -> Validator a
mkMaybeRecursiveValidator prefix a2mb = Validator $ \a -> [prefix <> ":" <> name | name <- maybe [] validate (a2mb a)]
