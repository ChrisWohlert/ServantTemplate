
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}


module Views where


import Text.Blaze
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (map, main, html)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import Data.Aeson
import Data.Aeson.Types
import Models


data Welcome = Welcome

newtype Layout a = Layout a 

instance ToJSON Book

instance HtmlComponent a => ToMarkup (Layout a) where
    toMarkup = html

instance HtmlComponent a => HtmlComponent (Layout a) where
    html l@(Layout x) = docTypeHtml $ do
        htmlHead $ do
            htmlTitle "Heyo"
            link ! rel "stylesheet" ! href "dist/css/shared.css"
        body $ do
            h1 "Layout"
            html x
            p "help"
            insertScripts
                where
                    insertScripts = mconcat $ map toScript (scripts l ++ scripts x)
                    toScript (Script src') = script ! type_ "text/javascript" ! src src' $ ""
    scripts l = [Script "/dist/js/common.js", Script "/dist/js/shared.js"]

instance HtmlComponent Welcome where
    html Welcome = h1 "Welcome"

instance HtmlComponent Book where
    html (Book isbn title) = 
        d ! class_ "book" $ do
            p (toHtml isbn)
            p (toHtml title)

instance ToJSON a => ToJSON (Layout a) where
    toJSON (Layout x) = toJSON x

instance HtmlComponent [Book] where
    html xs = mconcat $ map html xs

class HtmlComponent a where
    html :: a -> Html
    scripts :: a -> [Script]
    scripts x = []

data Script = Script AttributeValue

d = Text.Blaze.Html5.div
htmlHead = Text.Blaze.Html5.head
htmlTitle = Text.Blaze.Html5.title