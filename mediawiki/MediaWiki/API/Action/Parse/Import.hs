--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.Parse.Import
-- Description : Serializing Parse requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Serializing Parse requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Action.Parse.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Action.Parse

import Text.XML.Light.Types
import Text.XML.Light.Proc   ( strContent )

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) ParseResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe ParseResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "parse" es1
  let es = children p
  let txt = fromMaybe "" (pNode "text" es >>= return.strContent)
  let rev = pAttr "revid" p
  let ll  = fmap (mapMaybe xmlLL) (fmap children $ pNode "langlinks" es)
  let ca  = fmap (mapMaybe xmlCat) (fmap children $ pNode "categories" es)
  let li  = fmap (mapMaybe xmlLi) (fmap children $ pNode "links" es)
  let te  = fmap (mapMaybe xmlTe) (fmap children $ pNode "templates" es)
  let im  = fmap (mapMaybe xmlIm) (fmap children $ pNode "images" es)
  let ex  = fmap (mapMaybe xmlEx) (fmap children $ pNode "externallinks" es)
  let se  = fmap (mapMaybe xmlSe) (fmap children $ pNode "sections" es)
  return emptyParseResponse
           { parText = txt
	   , parRevId = rev
	   , parLangLinks = ll
	   , parCategories = ca
	   , parLinks = li
	   , parTemplates = te
	   , parImages = im
	   , parExternalLinks = ex
	   , parSections = se
	   }

xmlLL :: Element -> Maybe LanguageLink
xmlLL e = do
   guard (elName e == nsName "ll")
   let lng    = fromMaybe "en"  $ pAttr "lang" e
   return LanguageLink{laLang=lng,laLink=strContent e}

xmlCat :: Element -> Maybe CategoryLink
xmlCat e = do
   guard (elName e == nsName "cl")
   let sk    = fromMaybe ""  $ pAttr "sortkey" e
   return CategoryLink{caSortKey=sk,caLink=strContent e}

xmlLi :: Element -> Maybe Link
xmlLi e = do
   guard (elName e == nsName "pl")
   let ex   = isJust (pAttr "exists" e)
   let ns   = fromMaybe mainNamespace $ pAttr "ns" e
   return Link{liNamespace=ns,liExists=ex,liLink=strContent e}


xmlTe :: Element -> Maybe Link
xmlTe e = do
   guard (elName e == nsName "tl")
   let ex   = isJust (pAttr "exists" e)
   let ns   = fromMaybe mainNamespace $ pAttr "ns" e 
   return Link{liNamespace=ns,liExists=ex,liLink=strContent e}

xmlIm :: Element -> Maybe URLString
xmlIm e = do
   guard (elName e == nsName "img")
   return (strContent e)

xmlEx :: Element -> Maybe URLString
xmlEx e = do
   guard (elName e == nsName "el")
   return (strContent e)

xmlSe :: Element -> Maybe TOCSection
xmlSe e = do
   guard (elName e == nsName "s")
   let tlev = fromMaybe 0 $ pAttr "toclevel" e >>= readMb
   let lev = fromMaybe 0 $ pAttr "level" e >>= readMb
   let lin = fromMaybe "" $ pAttr "line" e
   let num = fromMaybe "" $ pAttr "number" e
   return TOCSection{tocTocLevel=tlev,tocLevel=lev,tocLine=lin,tocNumber=num}

