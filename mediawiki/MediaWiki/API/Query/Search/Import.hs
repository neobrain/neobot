module MediaWiki.API.Query.Search.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.Search

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) SearchResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe SearchResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "search" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "search" "sroffset"
  return emptySearchResponse{srPages=ps,srContinue=cont}

xmlPage :: Element -> Maybe PageTitle
xmlPage e = do
   guard (elName e == nsName "p")
   let ns     = fromMaybe "0" $ pAttr "ns" e
   let tit    = fromMaybe ""  $ pAttr "title" e
   let pid    = pAttr "pageid" e
   return emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=pid}
