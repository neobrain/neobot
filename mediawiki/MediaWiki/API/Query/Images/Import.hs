module MediaWiki.API.Query.Images.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.Images

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) ImagesResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe ImagesResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "pages" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "imageinfo" "iistart"
  return emptyImagesResponse{imLinks=ps,imContinue=cont}

xmlPage :: Element -> Maybe (PageTitle,[PageTitle])
xmlPage e = do
  guard (elName e == nsName "page")
  let pid     = fmap read $ pAttr "pageid" e
  let ns      = fromMaybe mainNamespace $ pAttr "ns" e
  let tit     = fromMaybe ""  $ pAttr "title" e
  let es = children e
  ls <- fmap (mapMaybe xmlIm) (fmap children $ pNode "images" es)
  let pg = emptyPageTitle{pgNS=ns,pgMbId=pid,pgTitle=tit}
  return (pg, ls)

xmlIm :: Element -> Maybe PageTitle
xmlIm e = do
   guard (elName e == nsName "im")
   let ns     = fromMaybe ns_IMAGE $  pAttr "ns" e
   let ti     = fromMaybe "" $ pAttr "title" e
   let pid    = pAttr "pageid" e
   return emptyPageTitle{pgNS=ns,pgTitle=ti,pgMbId=pid}
