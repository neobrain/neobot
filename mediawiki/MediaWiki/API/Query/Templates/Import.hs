module MediaWiki.API.Query.Templates.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.Templates

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) TemplatesResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe TemplatesResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "pages" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "templates" "tlcontinue"
  return emptyTemplatesResponse{tlPages=ps,tlContinue=cont}

xmlPage :: Element -> Maybe (PageTitle, [PageTitle])
xmlPage e = do
   guard (elName e == nsName "page")
   let ns     = fromMaybe "0" $ pAttr "ns" e
   let tit    = fromMaybe ""  $ pAttr "title" e
   let pid    = pAttr "pageid" e
   ps <- fmap (mapMaybe xmlT) (fmap children $ pNode "templates" (children e))
   return (emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=pid}, ps)
 where
  xmlT p = do
    guard (elName p == nsName "tl")
    let ns     = fromMaybe "0" $ pAttr "ns" p
    let tit    = fromMaybe ""  $ pAttr "title" p
    let pid    = pAttr "pageid" p
    return emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=pid}
