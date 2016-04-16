{-# LANGUAGE OverloadedStrings #-}

module ReadmeGen.Views.New where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

render errors =
  H.html $ do
    H.head $ do
      H.title "Readme Generator"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/readmegen.css"
    H.body $ do
      H.h2 ! A.class_ "header" $ "Readme Generator"
      H.h3 ! A.class_ "title" $ "Readme Generator"
      H.div ! A.class_ "container" $ do
        mapM_ renderErrors errors
        H.form ! A.class_ "form" ! A.method "post" ! A.action "/readme/" $ do
          H.table $ do
            H.tr $ do
              H.td $ H.label "Category"
	      H.td $ H.select ! A.name "category" $ do
	        H.option "bugfix"
	        H.option "feature"
	        H.option "security"
	        H.option "irrelevant"
            H.tr $ do
              H.td $ H.label "Section"
	      H.td $ H.select ! A.name "section" $ do
	        H.option "gui"
	        H.option "ha"
	        H.option "system"
	        H.option "doc"
	        H.option "test"
	        H.option "upgrade"
            H.tr $ do
              H.td $ H.label "Severity"
	      H.td $ H.select ! A.name "severity" $ do
	        H.option "minorfew"
	        H.option "minormany"
	        H.option "majorfew"
	        H.option "majormany"
	        H.option "nochange"
	        H.option "better"
            H.tr $ do
              H.td $ H.label "Type"
	      H.td $ H.select ! A.name "readme_type" $ do
	        H.option "Problem"
	        H.option "New Feature"
            H.tr $ do
              H.td $ H.label "Summary De: "
              H.td $ H.input ! A.required "required" ! A.maxlength "60" !
                A.name "title_de"
            H.tr $ do
              H.td $ H.label "Text De: "
              H.td $ H.textarea ! A.name "text_de" ! A.cols "50" !
	        A.rows "10" $ ""
            H.tr $ do
              H.td $ H.label "Summary En: "
              H.td $ H.input ! A.required "required" ! A.maxlength "60" !
                A.name "title_en"
            H.tr $ do
              H.td $ H.label "Text En: "
              H.td $ H.textarea ! A.name "text_en" ! A.cols "50" !
	        A.rows "10" $ ""
          H.div ! A.class_ "btns" $ do
            H.a ! A.class_ "btn" ! A.href "/readme" $ "Back"
            H.input ! A.class_ "btn" ! A.type_ "submit" !
		A.value "Generate Readme"
  where renderErrors error = do
          H.p ! A.class_ "error" $ error
          H.br
