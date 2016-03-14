module MediaWiki.API.Query.Links.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.Links

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) LinksResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe LinksResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "pages" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "langlinks" "llcontinue"
  return emptyLinksResponse
          { plPages = ps
	  , plContinue = cont
	  }

xmlPage :: Element -> Maybe (PageTitle,[PageTitle])
xmlPage e = do
  guard (elName e == nsName "page")
  let pid     = fmap read $ pAttr "pageid" e
  let ns      = fromMaybe mainNamespace $ pAttr "ns" e
  let tit     = fromMaybe ""  $ pAttr "title" e
  let es = children e
  ls <- fmap (mapMaybe xmlPageLink) (fmap children $ pNode "links" es)
  let pg = emptyPageTitle{pgNS=ns,pgMbId=pid,pgTitle=tit}
  return (pg,ls)

xmlPageLink :: Element -> Maybe PageTitle
xmlPageLink e = do
   guard (elName e == nsName "pl")
   let ns      = fromMaybe mainNamespace $ pAttr "ns" e
   let tit     = fromMaybe ""  $ pAttr "title" e
   let pid     = pAttr "pageid" e
   return emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=pid}
