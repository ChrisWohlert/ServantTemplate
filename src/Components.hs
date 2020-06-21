{-# LANGUAGE DataKinds             #-}
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
import Labels



data Layout a = Layout Menu a 

data Icon = Tag | New deriving(Show)

data MenuLink = MenuLink Icon Link Text

data Menu = LeftMenu Link Text [MenuLink]

type TableHeader = Text

data Table a = Table { tableHeaders :: [TableHeader], tableRows :: [a] }

newtype RowColumn = RowColumn String

data Row = Row { rowColumns :: [RowColumn] }


data InputField = InputField { iptype :: String, ipValue :: String, ipName :: String } 
                | InputLabelField LabelText InputField
                | InputListField [InputField]
                deriving (Show) 

                

