module MediaWiki.API.Query.AllMessages.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.AllMessages

import Text.XML.Light.Types
import Text.XML.Light.Proc   ( strContent )

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) AllMessagesResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe AllMessagesResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "pages" es)
  return emptyAllMessagesResponse{amsgMessages=ps}

xmlPage :: Element -> Maybe MessageInfo
xmlPage e = do
   guard (elName e == nsName "message")
   let nm    = fromMaybe "" $ pAttr "name" e
   let mi    = isJust (pAttr "missing" e)
   return emptyMessageInfo{msgiName=nm,msgiMissing=mi,msgiContent=strContent e}
   
