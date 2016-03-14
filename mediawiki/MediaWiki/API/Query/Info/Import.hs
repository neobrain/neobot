module MediaWiki.API.Query.Info.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.Info

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) InfoResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe InfoResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "pages" es)
  return emptyInfoResponse{infPages=ps}

xmlPage :: Element -> Maybe InfoPage
xmlPage e = do
  guard (elName e == nsName "page")
  let touched = fromMaybe "" $ pAttr "touched" e
  let rvid    = fromMaybe "0"  $ pAttr "lastrevid" e
  let cnt     = fromMaybe 0  $ (pAttr "counter" e >>= readMb)
  let len     = fromMaybe 0  $ (pAttr "length" e  >>= readMb)
  let ns      = fromMaybe "0" $ pAttr "ns" e
  let tit     = fromMaybe ""  $ pAttr "title" e
  let pid     = pAttr "pageid" e
  let miss    = fromMaybe False  $ fmap (const True) $ pAttr "missing" e
  let red     = fromMaybe False  $ fmap (const True) $ pAttr "redirect" e
  let new     = fromMaybe False  $ fmap (const True) $ pAttr "new" e
  let edTok   = pAttr "edittoken" e
  let deTok   = pAttr "deletetoken" e
  let prTok   = pAttr "protecttoken" e
  let moTok   = pAttr "movetoken" e
  return emptyInfoPage
    { infPage = emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=pid,pgMissing=miss}
    , infTouched   = touched
    , infLastRevId = rvid
    , infCounter   = cnt
    , infLength    = len
    , infIsRedirect = red
    , infIsNew      = new
    , infEditTok    = edTok
    , infDeleteTok  = deTok
    , infProtectTok  = prTok
    , infMoveTok     = moTok
    , infProtection  = []
    }
