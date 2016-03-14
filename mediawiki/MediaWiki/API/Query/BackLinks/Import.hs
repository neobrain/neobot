module MediaWiki.API.Query.BackLinks.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.BackLinks

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) BackLinksResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe BackLinksResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "backlinks" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "backlinks" "blcontinue"
  return emptyBackLinksResponse{blLinks=ps,blContinue=cont}

xmlPage :: Element -> Maybe PageTitle
xmlPage e = do
   guard (elName e == nsName "bl")
   let ns     = fromMaybe "0" $ pAttr "ns" e
   let tit    = fromMaybe ""  $ pAttr "title" e
   let mbpid  = pAttr "pageid" e
   return emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=mbpid}
