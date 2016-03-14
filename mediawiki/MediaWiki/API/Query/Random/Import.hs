module MediaWiki.API.Query.Random.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.Random

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) RandomPagesResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe RandomPagesResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "random" es)
  return emptyRandomPagesResponse{rnPages=ps}

xmlPage :: Element -> Maybe PageTitle
xmlPage e = do
   guard (elName e == nsName "page")
   let ns     = fromMaybe "0" $ pAttr "ns" e
   let tit    = fromMaybe ""  $ pAttr "title" e
   let pid    = pAttr "pageid" e
   return emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=pid}
