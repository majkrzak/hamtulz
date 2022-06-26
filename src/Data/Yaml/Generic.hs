{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Yaml.Generic () where

import GHC.Generics
import Data.Text (pack, Text)
import Data.Yaml.Builder (YamlBuilder(YamlBuilder), ToYaml, toYaml, mapping, unYamlBuilder, string)


class GToYaml f where
  gToYaml :: f a -> YamlBuilder

instance GToYaml f => GToYaml (D1 d f) where
  gToYaml = gToYaml . unM1

instance Constructor c => GToYaml (C1 c U1) where
  gToYaml x = string $ pack $ conName x

instance (GToYaml x, GToYaml y) => GToYaml (x :+: y) where
  gToYaml (L1 l) = gToYaml l
  gToYaml (R1 r) = gToYaml r

instance (GToYamlPairs x, GToYamlPairs y) => GToYaml (C1 c (x :*: y)) where
  gToYaml = mapping . gToYamlPairs . unM1


class GToYamlPairs f where
  gToYamlPairs :: f a -> [(Text, YamlBuilder)]

instance (Selector s, GToYaml f) => GToYamlPairs (S1 s f) where
  gToYamlPairs x = [(pack (selName x), gToYaml (unM1 x))]

instance (GToYamlPairs x, GToYamlPairs y) => GToYamlPairs (x :*: y) where
  gToYamlPairs (l :*: r) = l' <> r'
    where
      l' = gToYamlPairs l
      r' = gToYamlPairs r


instance ToYaml a => GToYaml (K1 R a) where
  gToYaml x = toYaml $ unK1 x


instance {-# OVERLAPPABLE #-} (Generic a, GToYaml (Rep a)) => ToYaml a where
  toYaml = gToYaml . from

