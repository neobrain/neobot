--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.ExpandTemplates.Import
-- Description : Serializing ExpandTemplates requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Serializing ExpandTemplates requests.
--
--------------------------------------------------------------------
module MediaWiki.API.Action.ExpandTemplates.Import where

--import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Action.ExpandTemplates

import Text.XML.Light.Types
import Text.XML.Light.Proc   ( strContent )

import Control.Monad

stringXml :: String -> Either (String,[{-Error msg-}String]) ExpandTemplatesResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe ExpandTemplatesResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "expandtemplates" es1
  let xm = fmap strContent $ pNode "parsetree" es1
  return emptyExpandTemplatesResponse
      { etExpandedText = strContent p
      , etExpandedXml  = xm
      }
