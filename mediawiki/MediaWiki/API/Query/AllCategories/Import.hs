module MediaWiki.API.Query.AllCategories.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.AllCategories
import MediaWiki.API.Query.CategoryInfo
import MediaWiki.API.Query.CategoryInfo.Import ( xmlCI )

import Text.XML.Light.Types
import Text.XML.Light.Proc ( strContent )

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) AllCategoriesResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe AllCategoriesResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlCII) (fmap children $ pNode "allcategories" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "allcategories" "acfrom"
  return emptyAllCategoriesResponse{acCategories=ps,acContinue=cont}

xmlCII :: Element -> Maybe CategoryInfo
xmlCII e = do
  c <- xmlCI emptyPageTitle "c" e
  let tit = strContent e
  return c{ciPage=(ciPage c){pgTitle=tit}}
  
