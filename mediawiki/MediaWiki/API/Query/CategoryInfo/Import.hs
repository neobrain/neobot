module MediaWiki.API.Query.CategoryInfo.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.CategoryInfo

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) CategoryInfoResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe CategoryInfoResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "pages" es)
  return emptyCategoryInfoResponse{ciPages=ps}

xmlPage :: Element -> Maybe CategoryInfo
xmlPage e = do
   guard (elName e == nsName "page")
   let ns     = fromMaybe "0" $ pAttr "ns" e
   let tit    = fromMaybe ""  $ pAttr "title" e
   let pid    = pAttr "pageid" e
   let pg     = emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=pid}
   let cs = mapMaybe (xmlCI pg "categoryinfo") (children e)
   listToMaybe cs
   
xmlCI :: PageTitle -> String -> Element -> Maybe CategoryInfo
xmlCI pg tg e = do
   guard (elName e == nsName tg)
   let sz  = pAttr "size" e >>= readMb
   let psz = pAttr "pagesize" e >>= readMb
   let fi  = pAttr "files" e >>= readMb
   let su  = pAttr "subcats" e >>= readMb
   let hi  = isJust (pAttr "hidden" e)
   return emptyCategoryInfo
               { ciPage = pg
	       , ciSize = sz
	       , ciPageSize = psz
	       , ciFiles = fi
	       , ciSubCats = su
	       , ciHidden = hi
	       }

