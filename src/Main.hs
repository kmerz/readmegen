{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mconcat)
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import qualified ReadmeGen.Views.New
import qualified ReadmeGen.Views.Show

blaze = S.html . renderHtml

data Readme = Readme {
  title_de :: String,
  text_de :: String,
  title_en :: String,
  text_en :: String
} deriving (Show, Read)

readme = Readme { title_de = "Woop", text_de = "YEAD",
  title_en = "WHOOP", text_en = "XXX" }

main = S.scotty 3000 $ do
  S.get "/" $ S.redirect "/new"
  S.get "/readmegen.css" $ S.file "readmegen.css"
  S.get "/new" $ blaze $ ReadmeGen.Views.New.render []
  S.post "/readme" $ blaze $ ReadmeGen.Views.Show.render readme
