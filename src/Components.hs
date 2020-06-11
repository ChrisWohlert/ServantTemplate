{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}

module Components where

import Servant.Links
import Data.Text
import GHC.Generics       
import Labels



data Layout a = Layout Menu a 

data Icon = Tag | New deriving(Show)

data MenuLink = MenuLink Icon Link Text

data Menu = LeftMenu Link Text [MenuLink]

data Table a = Table { tableHeaders :: [Text], tableRows :: [a] }

newtype RowColumn = RowColumn String

data Row = Row { rowColumns :: [RowColumn] }


data InputField = InputField { iptype :: String, ipValue :: String, ipName :: String } 
                | LabelField LabelText InputField
                | ListField [InputField]
                deriving (Show) 

                


data FormInput (l :: LabelText) a = FormInput a deriving (Show, Generic)