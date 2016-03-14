module MediaWiki.API.Query.AllImages.Import where

import MediaWiki.API.Utils
import MediaWiki.API.Query.AllImages
import MediaWiki.API.Query.ImageInfo.Import ( xmlII )

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) AllImagesResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe AllImagesResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe (xmlII "img")) (fmap children $ pNode "allimages" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "allimages" "aifrom"
  return emptyAllImagesResponse{aiImages=ps,aiContinue=cont}
