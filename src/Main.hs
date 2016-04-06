{-# LANGUAGE OverloadedStrings #-}

import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import qualified ReadmeGen.Views.New
import qualified ReadmeGen.Views.Show

data ReadmeD = ReadmeD {
  title_de :: String,
  text_de :: String,
  title_en :: String,
  text_en :: String
} deriving (Show, Read)

blaze = S.html . renderHtml

main = S.scotty 3000 $ do
  S.get "/" $ S.redirect "/new"
  S.get "/readmegen.css" $ S.file "readmegen.css"
  S.get "/new" $ blaze $ ReadmeGen.Views.New.render []
  S.post "/readme" $ do
    titlede <- S.param "title_de"
    textde <- S.param "text_de"
    titleen <- S.param "title_en"
    texten <- S.param "text_en"
    let readme = ReadmeD {
      title_de = titlede, text_de = textde, title_en = titleen,
      text_en = texten }
    blaze $ ReadmeGen.Views.Show.render $ toReadme readme

toReadme :: ReadmeD -> String
toReadme readme = (title_de readme) ++ "\n\n" ++
  (text_de readme) ++ "\n\n" ++ (title_en readme) ++ "\n\n" ++ (text_en readme)
