{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PolyKinds             #-}

module Labels where
  
import Data.Proxy       


data LabelText = LabelUsername | LabelPassword deriving (Show)

class LabelVal a where
    labelVal :: Proxy a -> LabelText

instance LabelVal 'LabelUsername where
  labelVal _ = LabelUsername

instance LabelVal 'LabelPassword where
  labelVal _ = LabelPassword