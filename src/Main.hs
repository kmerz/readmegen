{-# LANGUAGE OverloadedStrings #-}

import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import qualified ReadmeGen.Views.New
import qualified ReadmeGen.Views.Show

data ReadmeD = ReadmeD {
  category :: String,
  section :: String,
  severity :: String,
  readme_type :: String,
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
    category <- S.param "category"
    section <- S.param "section"
    severity <- S.param "severity"
    readme_type <- S.param "readme_type"
    titlede <- S.param "title_de"
    textde <- S.param "text_de"
    titleen <- S.param "title_en"
    texten <- S.param "text_en"
    let readme = ReadmeD {
      category = category,
      section = section,
      severity = severity,
      readme_type = readme_type,
      title_de = titlede,
      text_de = textde,
      title_en = titleen,
      text_en = texten }
    blaze $ ReadmeGen.Views.Show.render $ toReadme readme

toReadme :: ReadmeD -> String
toReadme readme = "category:" ++ (category readme) ++ "\n" ++
  "section:" ++ (section readme) ++ "\n" ++
  "severity:" ++ (severity readme) ++ "\n" ++
  "summary_de: " ++ (title_de readme) ++ "\n" ++
  "summary_en: " ++ (title_en readme) ++ "\n\n" ++
  (readme_type readme) ++ ":\n" ++
  (text_de readme) ++ "\n\n" ++
  (readme_type readme) ++ ":\n" ++
  (text_en readme)
