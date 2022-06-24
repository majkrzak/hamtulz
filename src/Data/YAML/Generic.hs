{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.YAML.Generic () where

import Data.YAML
import GHC.Generics
import Data.Text (pack)
import Data.YAML.Event (untagged)


class GToYAML f where
  gToYAML :: f a -> Node ()

instance GToYAML f => GToYAML (D1 d f) where
  gToYAML = gToYAML . unM1

instance GToYAML f => GToYAML (C1 d f) where
  gToYAML = gToYAML . unM1

instance {-# OVERLAPS #-} Constructor c => GToYAML (C1 c U1) where
  gToYAML x = Scalar () $ SStr $ pack $ conName x

instance (GToYAML x, GToYAML y) => GToYAML (x :+: y) where
  gToYAML (L1 l) = gToYAML l
  gToYAML (R1 r) = gToYAML r

instance (Selector s, GToYAML f) => GToYAML (S1 s f) where
  gToYAML x = mapping [pack (selName x) .= gToYAML  (unM1 x)]

instance (GToYAML x, GToYAML y) => GToYAML (x :*: y) where
  gToYAML (l :*: r) = Mapping () untagged (l' <> r')
    where
      Mapping _ _ l' = gToYAML l
      Mapping _ _ r' = gToYAML r


instance ToYAML a => GToYAML (K1 R a) where
  gToYAML x = toYAML $ unK1 x


instance {-# OVERLAPPABLE #-} (Generic a, GToYAML (Rep a)) => ToYAML a where
  toYAML = gToYAML . from
