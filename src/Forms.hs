{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE PolyKinds             #-}

module Forms where

import GHC.Generics       
import Data.Proxy                                     




data InputField = InputField { iptype :: String, ipValue :: String, ipName :: String } 
                | LabelField LabelText InputField
                | ListField [InputField]
                deriving (Show) 

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


data LabelText = LabelUsername | LabelPassword deriving (Show)

data FormInput (l :: LabelText) a = FormInput a deriving (Show, Generic)

data Test = TestUser { tusername :: FormInput 'LabelUsername String, tpassword :: FormInput LabelPassword String, tage :: Int, tsession :: Session } deriving (Show, Generic)

data Session = Session { sid :: String, sexpires :: String } deriving (Show, Generic)

instance ToForm Char where
  toForm x = []

instance ToForm Test
instance ToForm Session

instance {-# Overlaps #-} (Selector s, ToInputField t) => ToForm' (M1 S s (K1 R t)) where
  toForm' x@(M1 e) = [toInputField (selName x) (unK1 e)]

class ToInputField a where
  toInputField :: String -> a -> InputField

instance ToInputField String where
  toInputField n v = InputField "text" v n

instance (ToInputField a, LabelVal l) => ToInputField (FormInput l a) where
  toInputField n (FormInput v) = LabelField (labelVal (Proxy :: Proxy l)) (toInputField n v)

instance ToInputField Int where
  toInputField n v = InputField "number" (show v) n
  
instance ToInputField Session where
  toInputField n s = ListField (toForm s)

class LabelVal a where
  labelVal :: Proxy a -> LabelText

instance LabelVal 'LabelUsername where
  labelVal _ = LabelUsername

instance LabelVal 'LabelPassword where
  labelVal _ = LabelPassword