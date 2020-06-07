{-# LANGUAGE DeriveGeneric #-}

module Models where

import GHC.Generics



data User = User { username :: String, userEmail :: String }
   deriving (Eq, Show, Read, Generic)

data Book = Book { bookISBN :: String
                 , bookTitle :: String
                 } deriving (Generic, Show)