{-# LANGUAGE OverloadedStrings #-}

module ReadmeGen.Views.Index where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Monoid ((<>))

render items isSearch =
  H.html $ do
    H.head $ do
      H.title "Readme Generator"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/readmegen.css"
    H.body $ do
      H.h2 ! A.class_ "header" $ "Readme Generator"
      H.form ! A.class_ "form" ! A.method "get" ! A.action "/search" $ do
        H.input ! A.required "required" ! A.maxlength "60" !
          A.placeholder "e.g 23456" ! A.name "bug"
        H.input ! A.class_ "btn" ! A.type_ "submit" !  A.value "Find"
      H.div ! A.class_ "container" $ do
        H.a ! A.class_ "btn" ! A.href "/readme/new" $ "New Readme"
	indexLink
        H.table ! A.class_ "table" $ mapM_ renderLn items
  where renderLn (id, bug, title) = H.tr $ do
          H.td $ H.a ! A.href ("/readme/" <> H.stringValue (show id)) $
            H.toHtml ("#" ++ bug)
          H.td $ H.a ! A.href ("/readme/" <> H.stringValue (show id)) $
            H.toHtml (title)
        indexLink = if isSearch
          then H.a ! A.class_ "btn" ! A.href "/readme" $ "Back"
	  else ""
