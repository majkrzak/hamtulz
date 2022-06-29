{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Data.Yaml.Generic () where

import GHC.Generics
import Data.Text (pack, Text)
import Data.Yaml.Builder (YamlBuilder, ToYaml, toYaml, mapping, string)
import Data.Data (Proxy(Proxy))


class GToYaml f where
  gToYaml :: f a -> YamlBuilder

instance GToYaml f => GToYaml (D1 d f) where
  gToYaml = gToYaml . unM1

instance (GToYaml x, GToYaml y) => GToYaml (x :+: y) where
  gToYaml (L1 l) = gToYaml l
  gToYaml (R1 r) = gToYaml r

instance (GToYamlPairs x, GToYamlPairs y) => GToYaml (C1 c (x :*: y)) where
  gToYaml = mapping . gToYamlPairs . unM1


class GToYamlPairs f where
  gToYamlPairs :: f a -> [(Text, YamlBuilder)]

instance (Selector s, GToYaml f) => GToYamlPairs (S1 s f) where
  gToYamlPairs x = [(pack (selName x), gToYaml (unM1 x))]

instance {-# INCOHERENT #-} (Selector s, ToYaml a) => GToYamlPairs (S1 s (K1 i (Maybe a))) where
  gToYamlPairs (M1 (K1 Nothing)) = []
  gToYamlPairs x@(M1 (K1 (Just a))) = [(pack (selName x), toYaml a)]

instance (GToYamlPairs x, GToYamlPairs y) => GToYamlPairs (x :*: y) where
  gToYamlPairs (l :*: r) = l' <> r'
    where
      l' = gToYamlPairs l
      r' = gToYamlPairs r



data ToYamlMode = ToYamlGeneric | ToYamlShow

class ToYaml' (flag :: ToYamlMode) a where
  toYaml' :: Proxy flag -> a -> YamlBuilder

instance ToYaml a => GToYaml (K1 R a) where
  gToYaml x = toYaml $ unK1 x

instance Show a => ToYaml' 'ToYamlShow a where
   toYaml' _ = string . pack . show

instance (Generic a, GToYaml (Rep a)) => ToYaml' 'ToYamlGeneric a where
   toYaml' _ = gToYaml . from

type family FindHowToConvertToYaml rep :: ToYamlMode where
  FindHowToConvertToYaml (D1 _ (_ :+: _)) = 'ToYamlShow
  FindHowToConvertToYaml _ = 'ToYamlGeneric


instance {-# OVERLAPPABLE #-}  (ToYaml' (FindHowToConvertToYaml (Rep a)) a) => ToYaml a where
  toYaml = toYaml' (Proxy @(FindHowToConvertToYaml (Rep a)))