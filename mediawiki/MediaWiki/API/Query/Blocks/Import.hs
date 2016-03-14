module MediaWiki.API.Query.Blocks.Import where

import MediaWiki.API.Utils
import MediaWiki.API.Query.Blocks

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) BlocksResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe BlocksResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  ps <- fmap (mapMaybe xmlB) (fmap children $ pNode "blocks" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "blocks" "blfrom"
  return emptyBlocksResponse{bkBlocks=ps,bkContinue=cont}

xmlB :: Element -> Maybe BlockInfo
xmlB e = do
   guard (elName e == nsName "block")
   let i      = pAttr "id" e
   let usr    = pAttr "user" e
   let by     = pAttr "by" e
   let ts     = pAttr "timestamp" e
   let ex     = pAttr "expiry" e
   let re     = pAttr "reason" e
   let ras    = pAttr "rangestart" e
   let rae    = pAttr "rangeend" e
   let isa    = isJust $ pAttr "automatic" e
   let isan   = isJust $ pAttr "anononly" e
   let isnc   = isJust $ pAttr "nocreate" e
   let isab   = isJust $ pAttr "autoblock" e
   let isne   = isJust $ pAttr "noemail" e
   let ishi   = isJust $ pAttr "hidden" e
   return emptyBlockInfo
    { bkId = i
    , bkUser = usr
    , bkBy   = by
    , bkTimestamp = ts
    , bkExpiry = ex
    , bkReason = re
    , bkRangeStart = ras
    , bkRangeEnd = rae
    , bkIsAuto = isa
    , bkIsAnonOnly  = isan
    , bkIsNoCreate  = isnc
    , bkIsAutoBlock  = isab
    , bkIsNoEmail  = isne
    , bkIsHidden  = ishi
    }
