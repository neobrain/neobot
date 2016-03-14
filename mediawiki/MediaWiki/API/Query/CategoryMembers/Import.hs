module MediaWiki.API.Query.CategoryMembers.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.CategoryMembers

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) CategoryMembersResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe CategoryMembersResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "categorymembers" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "categorymembers" "cmcontinue"
  return emptyCategoryMembersResponse{cmPages=ps,cmContinue=cont}

xmlPage :: Element -> Maybe PageTitle
xmlPage e = do
   guard (elName e == nsName "cm")
   let ns     = fromMaybe "0" $ pAttr "ns" e
   let tit    = fromMaybe ""  $ pAttr "title" e
   let mbpid  = pAttr "pageid" e
   return emptyPageTitle{pgNS = ns,pgTitle = tit,pgMbId = mbpid}
