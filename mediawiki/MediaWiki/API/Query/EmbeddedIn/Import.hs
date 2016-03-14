module MediaWiki.API.Query.EmbeddedIn.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.EmbeddedIn

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) EmbeddedInResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe EmbeddedInResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "embeddedin" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "embeddedin" "eicontinue"
  return emptyEmbeddedInResponse{eiLinks=ps,eiContinue=cont}

xmlPage :: Element -> Maybe PageTitle
xmlPage e = do
   guard (elName e == nsName "ei")
   let ns     = fromMaybe "0" $ pAttr "ns" e
   let tit    = fromMaybe ""  $ pAttr "title" e
   let mbpid  = pAttr "pageid" e
   return emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=mbpid}
