{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE PolyKinds             #-}

module Forms where

import GHC.Generics                                   
import Components
import Labels
import Data.Proxy       


class ToForm' f where
  toForm' :: f p -> [InputField]

class ToForm a where
  toForm :: a -> [InputField]
  default toForm :: (Generic a, ToForm' (Rep a)) => a -> [InputField]
  toForm x = toForm' (from x)

instance ToForm' V1 where
  toForm' = undefined

instance ToForm' U1 where
  toForm' U1 = []

instance (ToForm' f, ToForm' g) => ToForm' (f :+: g) where
  toForm' (L1 x) = toForm' x
  toForm' (R1 x) = toForm' x

instance (ToForm' f, ToForm' g) => ToForm' (f :*: g) where
  toForm' (x :*: y) = toForm' x ++ toForm' y

instance (ToForm c) => ToForm' (K1 i c) where
  toForm' (K1 x) = toForm x

instance (ToForm' f) => ToForm' (M1 i t f) where
  toForm' (M1 x) = toForm' x


instance ToForm Char where
  toForm x = []

instance {-# Overlaps #-} (Selector s, ToInputField t) => ToForm' (M1 S s (K1 R t)) where
  toForm' x@(M1 e) = [toInputField (selName x) (unK1 e)]

class ToInputField a where
  toInputField :: String -> a -> InputField

instance ToInputField String where
  toInputField n v = InputField "text" v n

instance (ToInputField a, LabelVal l) => ToInputField (LabelField l a) where
  toInputField n (LabelField v) = InputLabelField (labelVal (Proxy :: Proxy l)) (toInputField n v)

instance ToInputField Int where
  toInputField n v = InputField "number" (show v) n