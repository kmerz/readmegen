{-# LANGUAGE OverloadedStrings #-}

module ReadmeGen.Views.Index where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Monoid ((<>))

render items =
  H.html $ do
    H.head $ do
      H.title "Readme Generator"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/readmegen.css"
    H.body $ do
      H.h2 ! A.class_ "header" $ "Readme Generator"
      H.div ! A.class_ "container" $ do
        H.a ! A.class_ "btn" ! A.href "/readme/new" $ "New Readme"
        H.table ! A.class_ "table" $ mapM_ renderLn items
  where renderLn i = H.tr $ do
          H.td $ H.a ! A.href ("/readme/" <> H.stringValue (show $ fst i)) $
            H.toHtml ("#" ++ (snd i))
