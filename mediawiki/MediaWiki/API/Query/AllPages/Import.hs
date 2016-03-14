module MediaWiki.API.Query.AllPages.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.AllPages

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) AllPagesResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe AllPagesResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "pages" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "allpages" "gapfrom"
  return emptyAllPagesResponse{apLinks=ps,apContinue=cont}

xmlPage :: Element -> Maybe PageTitle
xmlPage e = do
   guard (elName e == nsName "page")
   let ns     = fromMaybe "0" $ pAttr "ns" e
   let tit    = fromMaybe ""  $ pAttr "title" e
   let pid    = pAttr "pageid" e
    -- more to follow, I'm also seeing these with 1.13:
   let tou    = pAttr "touched" e
   let las    = pAttr "lastrevid" e
   let views  = pAttr "counter" e
   let len    = pAttr "length" e
   return emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=pid}
