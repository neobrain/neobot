module MediaWiki.API.Query.Revisions.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.Revisions

import Text.XML.Light.Types
import Text.XML.Light.Proc   ( strContent )

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) RevisionsResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe RevisionsResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "pages" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "revisions" "rvstartid"
  return emptyRevisionsResponse
          { rvPages    = ps
	  , rvContinue = cont
	  }

xmlPage :: Element -> Maybe (PageTitle,[Revision])
xmlPage e = do
  guard (elName e == nsName "page")
  let ns      = fromMaybe mainNamespace $ pAttr "ns" e
  let tit     = fromMaybe "" $ pAttr "title" e
  let pid     = pAttr "pageid" e
  let es = children e
  p  <- pNode "revisions" es
  let pg = emptyPageTitle{pgNS = ns, pgTitle=tit, pgMbId = pid}
  rs <- fmap (mapMaybe (xmlRevision pg)) (fmap children $ pNode "rev" (children p))
  return (pg, rs)

xmlRevision :: PageTitle -> Element -> Maybe Revision
xmlRevision pg e = do
  guard (elName e == nsName "page")
  let rid  = fromMaybe "" $ pAttr "revid" e
  let mino = isJust (pAttr "minor" e)
  let usr  = fromMaybe "" $ pAttr "user" e
  let anon = isJust (pAttr "anon" e)
  let ts   = fromMaybe "" $ pAttr "timestamp" e
  let size = fromMaybe 0 (pAttr "size" e >>= readMb)
  let com  = pAttr "comment" e
  let con  = case strContent e of { "" -> Nothing ; xs -> Just xs}
  return (emptyRevision pg)
             { revRevId = rid
	     , revIsMinor = mino
	     , revUser = usr
	     , revIsAnon = anon
	     , revTimestamp = ts
	     , revSize = size
	     , revComment = com
	     , revContent = con
	     }
