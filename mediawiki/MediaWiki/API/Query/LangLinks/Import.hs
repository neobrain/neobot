module MediaWiki.API.Query.LangLinks.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.LangLinks

import Text.XML.Light.Types
import Text.XML.Light.Proc   ( strContent )

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) LangLinksResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe LangLinksResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "pages" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "langlinks" "llcontinue"
  return emptyLangLinksResponse
          { llPages = ps
	  , llContinue = cont
	  }

xmlPage :: Element -> Maybe (PageTitle,[LangPageInfo])
xmlPage e = do
  guard (elName e == nsName "page")
  let pid     = fmap read $ pAttr "pageid" e
  let ns      = fromMaybe mainNamespace $ pAttr "ns" e
  let tit     = fromMaybe ""  $ pAttr "title" e
  let es = children e
  ls <- fmap (mapMaybe xmlLangLink) (fmap children $ pNode "langlinks" es)
  let pg = emptyPageTitle{pgNS=ns,pgMbId=pid,pgTitle=tit}
  return (pg, ls)

xmlLangLink :: Element -> Maybe LangPageInfo
xmlLangLink e = do
   guard (elName e == nsName "ll")
   let la     = fromMaybe "en" $  pAttr "lang" e
   let tit    = case strContent e of { "" -> Nothing ; xs -> Just xs}
   return emptyLangPageInfo{langName=la,langTitle=tit}
