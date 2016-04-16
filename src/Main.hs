{-# LANGUAGE OverloadedStrings #-}

import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.List.Split (splitOn)

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
      title_de = asciify $ titlede,
      text_de = justify $ asciify $ textde,
      title_en = asciify $ titleen,
      text_en = justify $ asciify $ texten }
    blaze $ ReadmeGen.Views.Show.render $ toReadme readme

toReadme :: ReadmeD -> String
toReadme readme = "category: " ++ (category readme) ++ "\n" ++
  "section: " ++ (section readme) ++ "\n" ++
  "severity: " ++ (severity readme) ++ "\n" ++
  "summary_de: " ++ (title_de readme) ++ "\n" ++
  "summary_en: " ++ (title_en readme) ++ "\n\n" ++
  (readme_type readme) ++ ":\n" ++
  (text_de readme) ++ "\n\n" ++
  (readme_type readme) ++ ":\n" ++
  (text_en readme)

asciify :: String -> String
asciify str = foldl (\acc c ->
    case c of
      'ü' -> acc ++ "ue"
      'Ü' -> acc ++ "Ue"
      'ä' -> acc ++ "ae"
      'Ä' -> acc ++ "Ae"
      'ö' -> acc ++ "oe"
      'Ö' -> acc ++ "Oe"
      'ß' -> acc ++ "ss"
      otherwise -> acc ++ c:""
  ) "" str

justify :: String -> String
justify str = foldl (\text word ->
  if ((length word) + (length $ last $ splitOn "\n" text)) < 75
  then if (length text) == 0 then text ++ word else text ++ " " ++ word
  else text ++ "\n" ++ word
  ) "" (words str)
