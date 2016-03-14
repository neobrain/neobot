module MediaWiki.API.Query.WatchList.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.WatchList

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) WatchListResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe WatchListResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  wl <- pNode "watchlist" es >>= xmlWatchList
  let cont = pNode "query-continue" es1 >>= xmlContinue "watchlist" "wlstart"
  return emptyWatchListResponse{wlWatch=wl,wlContinue=cont}

xmlWatchList :: Element -> Maybe WatchList
xmlWatchList e = do
   guard (elName e == nsName "watchlist")
   let ns     = fromMaybe "0" $ pAttr "ns" e
   let tit    = fromMaybe ""  $ pAttr "title" e
   let pid    = pAttr "pageid" e
   let rid    = pAttr "revid" e
   let usr    = pAttr "user" e
   let isa    = isJust (pAttr "anon" e)
   let isn    = isJust (pAttr "new" e)
   let ism    = isJust (pAttr "minor" e)
   let isp    = isJust (pAttr "patrolled" e)
   let ts     = pAttr "timestamp" e
   let len    = pAttr "newlen" e >>= readMb
   let leno   = pAttr "oldlen" e >>= readMb
   let co     = pAttr "comment" e
   let pg = emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=pid}
   return emptyWatchList
    { wlPage   = pg
    , wlRevId  = rid
    , wlUser   = usr
    , wlIsAnon = isa
    , wlIsNew  = isn
    , wlIsMinor = ism
    , wlIsPatrolled = isp
    , wlTimestamp = ts
    , wlLength  = len
    , wlOldLength = leno
    , wlComment = co
    }

