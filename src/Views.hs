
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds             #-}


module Views where


import Text.Blaze
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (map, main, html)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Svg
import Text.Blaze.Svg11 hiding (title, a, script, type_)
import Text.Blaze.Svg11.Attributes hiding (title, class_, d, a, type_)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import Data.Aeson
import Data.Aeson.Types
import Models
import Components
import Servant.Links
import Servant.API
import Tables
import Labels
import Data.Text hiding (map,)


data Welcome = Welcome

instance ToJSON Book
instance FromJSON Book

instance ToTableHeaders Book

instance HtmlComponent a => ToMarkup (Layout a) where
    toMarkup = html

instance HtmlComponent a => HtmlComponent (Layout a) where
    html l@(Layout menu x) = docTypeHtml $ do
        htmlHead $ do
            htmlTitle "Heyo"
            link ! rel "stylesheet" ! href "/dist/css/site.css"
        body ! class_ "preload" $ 
            d ! class_ "body-container" $ do
                html menu
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
    toJSON (Layout m x) = toJSON x

instance ToJSON a => ToJSON (Table a) where
    toJSON (Table hs rows) = toJSON rows

instance ToJSON a => ToJSON (LabelField l a) where
    toJSON (LabelField x) = toJSON x

instance FromJSON a => FromJSON (LabelField l a)

instance (HtmlComponent a) => HtmlComponent [a] where
    html xs = mconcat $ map html xs

instance (HtmlComponent a) => HtmlComponent (Table a) where
    html (Table hs rows) = table $ thead $ (mconcat $ map (th . html . toTableHeader) hs)

instance HtmlComponent TableHeader where
    html x = text x

instance (ToMarkup a) => ToMarkup (LabelField l a) where
    toMarkup (LabelField x) = toMarkup x


class HtmlComponent a where
    html :: a -> Html
    scripts :: a -> [Script]
    scripts x = []

data Script = Script AttributeValue







instance HtmlComponent Menu where
    html (LeftMenu link title menuLinks) = 
        d ! class_ "left-menu" $ do
            d ! class_ "left-menu-header" $ do
                d ! class_ "left-menu-header-toggle" $
                    button ! class_ "hamburger" ! class_ "hamburger--spin" ! class_ "is-active" $
                        sp ! class_ "hamburger-box" $
                            sp ! class_ "hamburger-inner" $ ""
                d ! class_ "left-menu-header-text" $
                    a ! href (textValue . toUrlPiece $ link) $ "Helld156dsdsdsasdsao"
            d ! class_ "left-menu-links" $
                d ! class_ "left-menu-group" $
                    mconcat $ map html menuLinks


instance HtmlComponent MenuLink where
    html (MenuLink icon link label) = 
        a ! href "/book" ! class_ "menu-link" $ do
            d ! class_ "menu-link-icon" $ html icon
            d ! class_ "menu-link-text" $ text label

instance HtmlComponent Icon where
    html = svgIcon . toLower . pack . show

svgIcon n = svg ! class_ "embla-icon" ! version "1.1" ! xmlSpace "http://www.w3.org/2000/svg" $
                    preEscapedText ("<use xlink:href=\"/dist/icons/sprite.symbol.svg#" <> n <> "\"></use>")

-- helpers

d = Text.Blaze.Html5.div
sp = Text.Blaze.Html5.span
htmlHead = Text.Blaze.Html5.head
htmlTitle = Text.Blaze.Html5.title