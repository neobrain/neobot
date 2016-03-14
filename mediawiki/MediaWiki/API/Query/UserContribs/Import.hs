module MediaWiki.API.Query.UserContribs.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.UserContribs

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) UserContribsResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe UserContribsResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "usercontribs" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "usercontribs" "ucstart"
  return emptyUserContribsResponse{ucPages=ps,ucContinue=cont}

xmlPage :: Element -> Maybe UserContrib
xmlPage e = do
   guard (elName e == nsName "item")
   let ns     = fromMaybe "0" $ pAttr "ns" e
   let tit    = fromMaybe ""  $ pAttr "title" e
   let pid    = pAttr "pageid" e
   let usr    = fromMaybe ""  $ pAttr "user" e
   let revid  = fromMaybe "0"  $ pAttr "revid" e
   let ts     = pAttr "timestamp" e
   let isn    = isJust (pAttr "new" e)
   let ism    = isJust (pAttr "minor" e)
   let ist    = isJust (pAttr "top" e)
   let co     = pAttr "comment" e
   return emptyUserContrib
     { ucoUser  = usr
     , ucoPage  = emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=pid}
     , ucoRevId = revid
     , ucoTimestamp = ts
     , ucoIsNew     = isn
     , ucoIsMinor   = ism
     , ucoIsTop     = ist
     , ucoComment   = co
     }

