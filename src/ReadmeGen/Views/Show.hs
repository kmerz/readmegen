{-# LANGUAGE OverloadedStrings #-}

module ReadmeGen.Views.Show where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

render (readme) =
  H.html $ do
    H.head $ do
      H.title "Readme Generator"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/readmegen.css"
    H.body $ do
      H.h2 ! A.class_ "header" $ "Readme Generator"
      H.h3 ! A.class_ "title" $ "Show Readme"
      H.div ! A.class_ "container" $ do
        H.textarea ! A.name "text_de" ! A.disabled "disabled" ! A.cols "50" !
	     A.rows "10" $ toHtml $ show readme
