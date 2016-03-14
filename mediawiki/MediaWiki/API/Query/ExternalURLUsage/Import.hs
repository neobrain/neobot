module MediaWiki.API.Query.ExternalURLUsage.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.ExternalURLUsage

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) ExternalURLUsageResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe ExternalURLUsageResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "exturlusage" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "exturlusage" "euoffset"
  return emptyExternalURLUsageResponse{euPages=ps,euContinue=cont}

xmlPage :: Element -> Maybe (URLString,PageTitle)
xmlPage e = do
   guard (elName e == nsName "eu")
   let ns     = fromMaybe "0" $ pAttr "ns" e
   let tit    = fromMaybe ""  $ pAttr "title" e
   let mbpid  = pAttr "pageid" e
   let url    = fromMaybe ""  $ pAttr "url" e
   return (url,emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=mbpid})


