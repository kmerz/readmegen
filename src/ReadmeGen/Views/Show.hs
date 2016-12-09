{-# LANGUAGE OverloadedStrings #-}

module ReadmeGen.Views.Show where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

host = "http://noodle.genua.de:3000"

fetch_url id bgnr =
  "curl " ++ host ++ "/readme/" ++ (id) ++ "/text > " ++
  "$(git rev-parse --show-toplevel)/patchreadmes/bugs/" ++ (bgnr) ++
  ".README && git add $(git rev-parse --show-toplevel)/patchreadmes/bugs/" ++
  (bgnr) ++ ".README"

render readme id bugnr =
  H.html $ do
    H.head $ do
      H.title "Readme Generator"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/readmegen.css"
    H.body $ do
      H.h2 ! A.class_ "header" $ "Readme Generator"
      H.h3 ! A.class_ "title" $ "Show Readme"
      H.a ! A.class_ "btn" !
        A.href (H.stringValue ("/readme/" ++ (id) ++ "/edit")) $ "Edit"
      H.a ! A.class_ "btn" !
        A.href (H.stringValue ("/readme")) $ "Index"
      H.div ! A.class_ "container" $ do
        H.table $ do
	  H.tr $ do
            H.td $ H.label "Fetch readme with:"
          H.tr $ do
            H.td $ H.input ! A.readonly "readonly" ! A.name "fetch-url" !
              A.class_ "copy-input" !
              A.value (H.stringValue (fetch_url id bugnr))
          H.tr $ do
            H.td $ H.textarea ! A.name "text_de" ! A.readonly "readonly" !
              A.cols "79" !  A.rows "50" $ toHtml $ readme
