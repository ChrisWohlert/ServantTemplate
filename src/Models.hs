{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds             #-}


module Models where

import GHC.Generics
import Labels


data User = User { username :: String, userEmail :: String }
   deriving (Eq, Show, Read, Generic)

data Book = Book { bookISBN :: LabelField LabelUsername String
                 , bookTitle :: LabelField LabelUsername String
                 } deriving (Generic, Show)