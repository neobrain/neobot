module MediaWiki.API.Query.ExternalLinks.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.ExternalLinks

import Text.XML.Light.Types
import Text.XML.Light.Proc ( strContent )

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) ExternalLinksResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe ExternalLinksResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es2 = children p
  p  <- pNode "pages" es2
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "page" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "extlinks" "eloffset"
  return emptyExternalLinksResponse{elPages=ps,elContinue=cont}

xmlPage :: Element -> Maybe (PageTitle,[URLString])
xmlPage e = do
   guard (elName e == nsName "page")
   let es = children e
   p <- pNode "extlinks" es
   let es1 = children p
   cs <- fmap (mapMaybe xmlEL) (fmap children $ pNode "el" es1)
   let ns     = fromMaybe "0" $ pAttr "ns" p
   let tit    = fromMaybe ""  $ pAttr "title" p
   let mbpid  = pAttr "pageid" p
   return (emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=mbpid}, cs)

xmlEL :: Element -> Maybe URLString
xmlEL e = do
   guard (elName e == nsName "el")
   return (strContent e)

