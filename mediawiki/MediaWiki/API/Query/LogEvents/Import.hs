module MediaWiki.API.Query.LogEvents.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.LogEvents

import Text.XML.Light.Types
import Text.XML.Light.Proc   ( strContent )

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) LogEventsResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe LogEventsResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "logevents" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "logevents" "lestart"
  return emptyLogEventsResponse
          { leEvents = ps
	  , leContinue = cont
	  }

xmlPage :: Element -> Maybe LogEvent
xmlPage e = do
  guard (elName e == nsName "item")
  let pid     = pAttr "pageid" e
  let ns      = fromMaybe mainNamespace $ pAttr "ns" e
  let tit     = fromMaybe ""  $ pAttr "title" e
  let ty      = pAttr "type" e
  let act     = pAttr "action" e
  let usr     = pAttr "user" e
  let ts      = pAttr "timestamp" e
  let co      = pAttr "comment" e
  let lid     = pAttr "logid" e
  let ps = mapMaybe xmlParam (children e)
  let pg = emptyPageTitle{pgNS=ns,pgMbId=pid,pgTitle=tit}
  return emptyLogEvent
              { levLogId = lid
	      , levPage  = pg
	      , levType  = ty
	      , levAction = act
	      , levParams = ps
	      , levUser   = usr
	      , levTimestamp = ts
	      , levComment = co
	      }

xmlParam :: Element -> Maybe LogEventParam
xmlParam e = do
  case qName (elName e) of
    "move" -> do
      let ns  = fromMaybe mainNamespace $ pAttr "new_ns" e
      let tit = fromMaybe "" $ pAttr "new_title" e
      let pid = pAttr "new_pageid" e
      return (LogEventMove emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=pid})
    "patrol" -> do
      let cur  = pAttr "cur"  e
      let pre  = pAttr "prev" e
      let auto = pAttr "auto" e
      return (LogEventPatrol{levPatrolCurrent=cur,levPatrolPrevious=pre,levPatrolAuto=auto})
    "rights" -> do
      let old  = pAttr "old"  e
      let new  = pAttr "new"  e
      return (LogEventRights{levRightsOld=old,levRightsNew=new})
    "block" -> do
      let dur = pAttr "duration" e
      let fla = pAttr "flags" e
      return (LogEventBlock{levBlockDuration=dur,levBlockFlags=fla})
    "param" -> return (LogEventParam (strContent e))
    tg -> return $
      LogEventOther{ levParamName  = tg
                   , levParamAttrs = map (\a -> (qName $ attrKey a,attrVal a))
		                         (elAttribs e)}
    

