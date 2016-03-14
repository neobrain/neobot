module MediaWiki.API.Query.Users.Import where

import MediaWiki.API.Utils
import MediaWiki.API.Query.Users

import Text.XML.Light.Types
import Text.XML.Light.Proc   ( strContent )

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) UsersResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe UsersResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "users" es)
  return emptyUsersResponse{usrUsers=ps}

xmlPage :: Element -> Maybe UsersInfo
xmlPage e = do
   guard (elName e == nsName "user")
   let nm     = pAttr "name" e
   let ec     = pAttr "editcount" e >>= readMb
   let re     = pAttr "registration" e
   let bl1    = pAttr "blockedby" e
   let bl2    = pAttr "blockreason" e
   let bl     = case (bl1,bl2) of { (Just a, Just b) -> Just (a,b) ; _ -> Nothing}
   let gs = fromMaybe [] $ fmap (mapMaybe xmlG) (fmap children $ pNode "groups" (children e))
   return emptyUsersInfo
            { usName = nm
	    , usEditCount = ec
	    , usRegDate = re
	    , usBlock = bl
	    , usGroups = gs
	    }
  where
   xmlG p = do
     guard (elName p == nsName "g")
     return (strContent p)


