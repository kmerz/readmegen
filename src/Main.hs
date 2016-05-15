{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import Control.Monad.Trans (liftIO)
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.List.Split (splitOn)
import Data.Time (UTCTime, getCurrentTime)
import System.Environment (getArgs)

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import qualified ReadmeGen.Views.New
import qualified ReadmeGen.Views.Show

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  ReadmeD
    bug String
    category String
    section String
    severity String
    readme_type String
    title_de String
    text_de String
    title_en String
    text_en String
    createdAt UTCTime
    deriving Show
  |]

blaze = S.html . renderHtml

initDb = runSqlite "readme.db" $ runMigration migrateAll

getPort :: [String] -> Int
getPort [] = 80
getPort args = head $ map (\ p -> (read p :: Int)) args

main :: IO ()
main = do
  args <- getArgs
  initDb
  scottySite $ getPort args

scottySite port = S.scotty port $ do
  S.get "/" $ S.redirect "/new"
  S.get "/readmegen.css" $ S.file "readmegen.css"
  S.get "/new" $ blaze $ ReadmeGen.Views.New.render []
  S.get "/readme/:id" $ do
    param_id <- S.param "id" :: S.ActionM String
    let id = toSqlKey (read param_id)
    readme <- getReadme id
    case readme of
      Just readme -> blaze $ ReadmeGen.Views.Show.render $ toReadme readme
      Nothing -> blaze $ ReadmeGen.Views.New.render []
  S.post "/readme" $ do
    bug <- S.param "bug"
    category <- S.param "category"
    section <- S.param "section"
    severity <- S.param "severity"
    readme_type <- S.param "readme_type"
    title_de <- S.param "title_de"
    text_de <- S.param "text_de"
    title_en <- S.param "title_en"
    text_en <- S.param "text_en"
    newId <- saveReadme bug category section severity readme_type title_de
      text_de title_en text_en
    readme <- getReadme newId
    case readme of
      Just readme -> blaze $ ReadmeGen.Views.Show.render $ toReadme readme
      Nothing -> blaze $ ReadmeGen.Views.New.render []

toReadme :: ReadmeD -> String
toReadme readme = "category: " ++ (readmeDCategory readme) ++ "\n" ++
  "section: " ++ (readmeDSection readme) ++ "\n" ++
  "severity: " ++ (readmeDSeverity readme) ++ "\n" ++
  "summary_de: " ++ (readmeDTitle_de readme) ++ "\n" ++
  "summary_en: " ++ (readmeDTitle_en readme) ++ "\n\n" ++
  (readmeDReadme_type readme) ++ ":\n" ++
  (readmeDText_de readme) ++ "\n\n" ++
  (readmeDReadme_type readme) ++ ":\n" ++
  (readmeDText_en readme)


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

saveReadme bug category section severity readme_type
  title_de text_de title_en text_en = do
  now <- liftIO $ getCurrentTime
  runSqlite "readme.db" $ insert $ ReadmeD bug category section severity
    readme_type title_de text_de title_en text_en now

getReadme id = runSqlite "readme.db" $ get id
