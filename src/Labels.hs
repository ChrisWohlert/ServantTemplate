{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}

module Labels where
  
import Data.Proxy       
import GHC.Generics       


data LabelText = LabelUsername | LabelPassword deriving (Show)

data LabelField (l :: LabelText) a = LabelField a deriving (Show, Generic)


class LabelVal a where
    labelVal :: Proxy a -> LabelText

instance LabelVal 'LabelUsername where
  labelVal _ = LabelUsername

instance LabelVal 'LabelPassword where
  labelVal _ = LabelPassword