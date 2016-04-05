{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mconcat)
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import qualified ReadmeGen.Views.New

blaze = S.html . renderHtml

main = S.scotty 3000 $ do
  S.get "/" $ S.redirect "/new"
  S.get "/readmegen.css" $ S.file "readmegen.css"
  S.get "/new" $ blaze $ ReadmeGen.Views.New.render []
