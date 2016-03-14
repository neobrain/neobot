module MediaWiki.API.Query.AllLinks.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.AllLinks

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) AllLinksResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe AllLinksResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlLink) (fmap children $ pNode "alllinks" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "alllinks" "alcontinue"
  return emptyAllLinksResponse{alLinks=ps,alContinue=cont}

xmlLink :: Element -> Maybe PageTitle
xmlLink e = do
   guard (elName e == nsName "l")
   let ns     = fromMaybe "0" $ pAttr "ns" e
   let tit    = fromMaybe ""  $ pAttr "title" e
   let pid    = pAttr "fromid" e
   return emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=pid}
