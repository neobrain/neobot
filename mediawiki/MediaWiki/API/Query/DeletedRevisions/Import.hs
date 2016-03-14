module MediaWiki.API.Query.DeletedRevisions.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.DeletedRevisions

import Text.XML.Light.Types
import Text.XML.Light.Proc   ( strContent )

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) DeletedRevisionsResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe DeletedRevisionsResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "revisions" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "deletedrevs" "drstart"
  return emptyDeletedRevisionsResponse{drRevisions=ps,drContinue=cont}

xmlPage :: Element -> Maybe DeletedRevision
xmlPage e = do
   guard (elName e == nsName "page")
   let ns     = fromMaybe "0" $ pAttr "ns" e
   let tit    = fromMaybe ""  $ pAttr "title" e
   let pid    = pAttr "pageid" e
   let pg     = emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=pid}
   let ts     = pAttr "timestamp" e
   let re     = pAttr "revid" e
   let usr    = pAttr "user" e
   let co     = pAttr "comment" e
   let ism     = isJust $ pAttr "minor" e
   let le     = pAttr "len" e >>= readMb
   let cts    = case (strContent e) of { "" -> Nothing; xs -> Just xs}
   let tok    = pAttr "token" e
   return emptyDeletedRevision
    { drPage      = pg
    , drTimestamp = ts
    , drRevId     = re
    , drUser      = usr
    , drComment   = co
    , drIsMinor   = ism
    , drLength    = le
    , drContent   = cts
    , drToken     = tok
    }
