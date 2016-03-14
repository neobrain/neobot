module MediaWiki.API.Query.ImageInfo.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.ImageInfo

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) ImageInfoResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe ImageInfoResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1 >>= (pNode "pages").children
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "page" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "imageinfo" "iistart"
  return emptyImageInfoResponse{iiPages=ps,iiContinue=cont}

xmlPage :: Element -> Maybe (PageTitle,[ImageInfo])
xmlPage e = do
   guard (elName e == nsName "page")
   let es = children e
   p <- pNode "imageinfo" es
   let es1 = children p
   cs <- fmap (mapMaybe (xmlII "ii")) (fmap children $ pNode "ii" es1)
   let ns     = fromMaybe "0" $ pAttr "ns" p
   let tit    = fromMaybe ""  $ pAttr "title" p
   let mbpid  = pAttr "pageid" p
   let miss   = isJust (pAttr "missing" p)
   let rep    = pAttr "imagerepository" p
   return (emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=mbpid}, cs)

xmlII :: String -> Element -> Maybe ImageInfo
xmlII tg p = do
   guard (elName p == nsName tg)
   let ts = fromMaybe nullTimestamp (pAttr "timestamp" p)
   let us = fromMaybe nullUser (pAttr "user" p)
   let wi = pAttr "width" p >>= readI
   let he = pAttr "height" p >>= readI
   let si = pAttr "size" p >>= readI
   let ur = pAttr "url" p
   let co = pAttr "comment" p
   let sh = pAttr "sha1" p
   let ar = pAttr "archivename" p
   let bi = pAttr "bitdepth" p >>= readI
   let mi = pAttr "mime" p
   return emptyImageInfo
    { iiTimestamp = ts
    , iiUser      = us
    , iiWidth     = wi
    , iiHeight    = he
    , iiSize      = si
    , iiURL       = ur
    , iiComment   = co
    , iiSHA1      = sh
    , iiArchive   = ar
    , iiBitDepth  = bi
    , iiMime      = mi
    }


readI :: String -> Maybe Int
readI s =
  case reads s of
    ((v,_):_) -> Just v
    _ -> Nothing


