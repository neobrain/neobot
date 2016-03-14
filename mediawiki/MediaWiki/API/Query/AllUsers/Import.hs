module MediaWiki.API.Query.AllUsers.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.AllUsers

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) AllUsersResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe AllUsersResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlUser) (fmap children $ pNode "allusers" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "allusers" "aufrom"
  return emptyAllUsersResponse{auUsers=ps,auContinue=cont}

xmlUser :: Element -> Maybe (UserName,Maybe Int, Maybe String)
xmlUser e = do
   guard (elName e == nsName "u")
   let ns     = fromMaybe "0" $ pAttr "ns" e
   let nm     = fromMaybe ""  $ pAttr "name" e
   let ec     = pAttr "editcount" e >>= \ x -> case reads x of { ((v,_):_) -> Just v; _ -> Nothing}
   let grps   = pAttr "groups" e
   return (nm,ec,grps)
