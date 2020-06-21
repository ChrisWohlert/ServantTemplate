{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE PolyKinds             #-}

module Rows where

import GHC.Generics       
import Data.Proxy                         
import Components    
import Labels        


class ToRow' f where
  toRow' :: f p -> [RowColumn]

class ToRow a where
  toRow :: a -> [RowColumn]
  default toRow :: (Generic a, ToRow' (Rep a)) => a -> [RowColumn]
  toRow x = toRow' (from x)

instance ToRow' V1 where
  toRow' = undefined

instance ToRow' U1 where
  toRow' U1 = []

instance (ToRow' f, ToRow' g) => ToRow' (f :+: g) where
  toRow' (L1 x) = toRow' x
  toRow' (R1 x) = toRow' x

instance (ToRow' f, ToRow' g) => ToRow' (f :*: g) where
  toRow' (x :*: y) = toRow' x ++ toRow' y

instance (ToRow c) => ToRow' (K1 i c) where
  toRow' (K1 x) = toRow x

instance (ToRow' f) => ToRow' (M1 i t f) where
  toRow' (M1 x) = toRow' x


instance ToRow Char where
  toRow x = []

instance {-# Overlaps #-} (Selector s, ToColumn t) => ToRow' (M1 S s (K1 R t)) where
  toRow' x@(M1 e) = [toColumn (unK1 e)]

class ToColumn a where
  toColumn :: a -> RowColumn

instance ToColumn String where
  toColumn = RowColumn

instance (ToColumn a) => ToColumn (LabelField l a) where
  toColumn (LabelField v) = toColumn v
