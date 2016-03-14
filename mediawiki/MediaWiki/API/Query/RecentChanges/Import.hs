module MediaWiki.API.Query.RecentChanges.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.RecentChanges

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) RecentChangesResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe RecentChangesResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "recentchanges" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "recentchanges" "rcstart"
  return emptyRecentChangesResponse
          { rchChanges  = ps
	  , rchContinue = cont
	  }

xmlPage :: Element -> Maybe RecentChange
xmlPage e = do
  guard (elName e == nsName "rc")
  let ty  = pAttr "type" e
  let ns  = fromMaybe mainNamespace $ pAttr "ns" e
  let tit = fromMaybe "" $ pAttr "title" e
  let pid = pAttr "pageid" e
  let ns_t = pAttr "new_ns" e
  let tit_t = pAttr "new_title" e
  let pid_t = pAttr "new_pageid" e
  let pg_to
       | any isJust [ns_t,tit_t,pid_t]
       = Just
          emptyPageTitle{ pgNS=fromMaybe mainNamespace ns_t
                        , pgTitle=fromMaybe "" tit_t
		        , pgMbId=pid_t
		        }
       | otherwise = Nothing
  let rcid = pAttr "rcid" e
  let revid = pAttr "revid" e
  let revido = pAttr "old_revid" e
  let usr = pAttr "user" e
  let isa = isJust (pAttr "anon" e)
  let isb = isJust (pAttr "bot" e)
  let isn = isJust (pAttr "new" e)
  let ism = isJust (pAttr "minor" e)
  let len = pAttr "newlen" e >>= readMb
  let leno = pAttr "oldlen" e >>= readMb
  let ts  = pAttr "timestamp" e
  let co  = pAttr "comment" e
  return emptyRecentChange
           { rchType = ty
	   , rchPage = emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=pid}
	   , rchPageTo = pg_to
	   , rchRcId   = rcid
	   , rchRevId  = revid
	   , rchRevOldId = revido
	   , rchUser = usr
	   , rchIsAnon = isa
	   , rchIsBot  = isb
	   , rchIsNew  = isn
	   , rchIsMinor = ism
	   , rchLength = len
	   , rchLengthOld = leno
	   , rchTimestamp = ts
	   , rchComment = co
	   }
