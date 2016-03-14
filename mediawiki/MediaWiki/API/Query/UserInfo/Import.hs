module MediaWiki.API.Query.UserInfo.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.UserInfo

import Text.XML.Light.Types
import Text.XML.Light.Proc   ( strContent )

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) UserInfoResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe UserInfoResponse
xml e = do
  guard (elName e == nsName "api")
  p0  <- pNode "query" (children e)
  p   <- pNode "userinfo" (children p0)
  let es = children p
  let grps = mapMaybe xmlGroup es
  let rs = mapMaybe xmlRights es
  let os = mapMaybe xmlOption es
  let rs1 = mapMaybe xmlRateLimit es
  let ec  = pAttr "editcount" p >>= readMb
  let nm  = fromMaybe nullUser $ pAttr "name" p
  let uid = fromMaybe "0" $ pAttr "id" p >>= readMb
  let isa = isJust (pAttr "anon" p)
  let hasm = isJust (pAttr "mesages" p)
  let bi   = xmlBlockInfo p
  let u = emptyUserInfo
            { uiName = nm
	    , uiId   = uid
	    , uiIsAnon = isa
	    , uiHasMessage = hasm
	    , uiBlocked = bi
	    , uiGroups = concat grps
	    , uiRights = concat rs
	    , uiOptions = concat os
	    , uiRateLimits = concat rs1
	    , uiEditCount = ec
	    }
  return emptyUserInfoResponse{uiUser=u}

xmlBlockInfo :: Element -> Maybe (String,String)
xmlBlockInfo e = do
  b <- pAttr "blockedby" e
  c <- pAttr "blockreason" e
  return (b,c)

xmlGroup :: Element -> Maybe [String]
xmlGroup e = do
   guard (elName e == nsName "groups")
   let es = children e
   let gs = mapMaybe xmlG es
   return gs
 where
  xmlG g = do
   guard (elName e == nsName "g")
   return (strContent g)

xmlRights :: Element -> Maybe [String]
xmlRights e = do
   guard (elName e == nsName "rights")
   let es = children e
   let gs = mapMaybe xmlR es
   return gs
  where
   xmlR g = do
    guard (elName e == nsName "r")
    return (strContent g)

xmlRateLimit :: Element -> Maybe [RateLimit]
xmlRateLimit e = do
   guard (elName e == nsName "ratelimits")
   let es = children e
   let gs = mapMaybe xmlR es
   return gs
 where
  xmlR g = do
   let es = children g
   p <- pNode "ip" es
   let hi = fromMaybe 0 $ pAttr "hits" p >>= readMb
   let se = fromMaybe 0 $ pAttr "seconds" p >>= readMb
   return RateLimit{rlName=qName (elName e),rlHits=hi,rlSeconds=se}

xmlOption :: Element -> Maybe [(String,String)]
xmlOption e = do
   guard (elName e == nsName "options")
   return (map (\a -> (qName (attrKey a), attrVal a)) (elAttribs e))
