module Components where

import Servant.Links
import Data.Text




data Layout a = Layout Menu a 

data Icon = Tag | New deriving(Show)

data MenuLink = MenuLink Icon Link Text

data Menu = LeftMenu Link Text [MenuLink]