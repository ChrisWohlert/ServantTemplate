{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE PolyKinds             #-}

module Tables where

import GHC.Generics                                   
import Components
import Labels
import Data.Proxy       
import Data.Text


class ToTableHeaders' f where
  toTableHeaders' :: [TableHeader]

class ToTableHeaders a where
  toTableHeaders :: [TableHeader]
  default toTableHeaders :: (Generic a, ToTableHeaders' (Rep a)) => [TableHeader]
  toTableHeaders = toTableHeaders'

instance ToTableHeaders' V1 where
  toTableHeaders' = undefined

instance ToTableHeaders' U1 where
  toTableHeaders' U1 = []

instance (ToTableHeaders' f, ToTableHeaders' g) => ToTableHeaders' (f :+: g) where
  toTableHeaders' (L1 x) = toTableHeaders' x
  toTableHeaders' (R1 x) = toTableHeaders' x

instance (ToTableHeaders' f, ToTableHeaders' g) => ToTableHeaders' (f :*: g) where
  toTableHeaders' (x :*: y) = toTableHeaders' x ++ toTableHeaders' y

instance (ToTableHeaders c) => ToTableHeaders' (K1 i c) where
  toTableHeaders' (K1 x) = toTableHeaders x

instance (ToTableHeaders' f) => ToTableHeaders' (M1 i t f) where
  toTableHeaders' (M1 x) = toTableHeaders' x


instance ToTableHeaders Char where
  toTableHeaders x = []

instance {-# Overlaps #-} (Selector s, ToTableHeader t) => ToTableHeaders' (M1 S s (K1 R t)) where
  toTableHeaders' x@(M1 e) = [toTableHeader (unK1 e)]

class ToTableHeader a where
  toTableHeader :: a -> TableHeader

instance ToTableHeader String where
  toTableHeader v = pack v

instance ToTableHeader TableHeader where
  toTableHeader v = v

instance (LabelVal l) => ToTableHeader (LabelField l a) where
  toTableHeader (LabelField v) =  pack $ show (labelVal (Proxy :: Proxy l))
