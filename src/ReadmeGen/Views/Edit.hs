{-# LANGUAGE OverloadedStrings #-}

module ReadmeGen.Views.Edit where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

render bug category section severity readme_type title_de text_de title_en
  text_en id errors =
  H.html $ do
    H.head $ do
      H.title "Readme Generator"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/readmegen.css"
    H.body $ do
      H.h2 ! A.class_ "header" $ "Readme Generator"
      H.div ! A.class_ "container" $ do
        mapM_ renderErrors errors
        H.form ! A.class_ "form" ! A.method "post" ! A.action (H.stringValue $
          "/readme/" ++ id) $ do
          H.table $ do
            H.tr $ do
              H.td $ H.label "Bug"
              H.td $ H.input ! A.required "required" ! A.maxlength "60" !
                A.placeholder "the referencing bug" ! A.name "bug" !
		A.value (H.stringValue bug)
            H.tr $ do
              H.td $ H.label "Category"
	      H.td $ selection "category" category ["bugfix", "feature",
	        "security", "irrelevant"]
            H.tr $ do
              H.td $ H.label "Section"
	      H.td $ selection "section" section [ "gui", "ha", "system",
	        "doc", "test", "upgrade"]
            H.tr $ do
              H.td $ H.label "Severity"
	      H.td $ selection "severity" severity [ "minorfew", "minormany",
	        "majorfew", "majormany", "nochange", "better"]
            H.tr $ do
              H.td $ H.label "Type"
	      H.td $ selection "readme_type" readme_type [ "Problem",
	        "New Feature" ]
            H.tr $ do
              H.td $ H.label ""
              H.td $ H.input ! A.required "required" ! A.maxlength "60" !
                A.placeholder "eine Zeile Zusammenfassung" ! A.name "title_de" !
                A.value (H.stringValue title_de)
            H.tr $ do
              H.td $ H.label ""
              H.td $ H.textarea ! A.required "required" ! A.name "text_de" !
                A.cols "50" !  A.rows "10" $ (H.string text_de)
            H.tr $ do
              H.td $ H.label ""
              H.td $ H.input ! A.required "required" ! A.maxlength "60" !
                A.placeholder "one line summary" ! A.name "title_en" !
                A.value (H.stringValue title_en)
            H.tr $ do
              H.td $ H.label ""
              H.td $ H.textarea ! A.required "required" ! A.name "text_en" !
                A.cols "50" !  A.rows "10" $ (H.string text_en)
          H.div ! A.class_ "btns" $ do
            H.input ! A.class_ "btn" ! A.type_ "submit" !
		A.value "Update Readme"
            H.a ! A.class_ "btn" !
              A.href (H.stringValue ("/readme/" ++ (id) ++ "")) $ "Back"
  where renderErrors error = do
          H.p ! A.class_ "error" $ error
          H.br
	selection name selected opts  = do
	  H.select ! A.name (H.stringValue name) $ do
            mapM_ (\opt ->
              if opt == selected
              then H.option (H.string opt) ! A.selected "selected"
	      else H.option (H.string opt)) opts
