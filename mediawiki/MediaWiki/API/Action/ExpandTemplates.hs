--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.ExpandTemplates
-- Description : Representing ExpandTemplates requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing ExpandTemplates requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Action.ExpandTemplates where

import MediaWiki.API.Types

data ExpandTemplatesRequest
 = ExpandTemplatesRequest
    { etTitle      :: Maybe PageName
    , etText       :: String
    , etGenXml     :: Maybe Bool
    }

emptyExpandTemplatesRequest :: ExpandTemplatesRequest
emptyExpandTemplatesRequest = ExpandTemplatesRequest
    { etTitle      = Nothing
    , etText       = ""
    , etGenXml     = Nothing
    }

data ExpandTemplatesResponse
 = ExpandTemplatesResponse
    { etExpandedText :: String
    , etExpandedXml  :: Maybe String
    }

emptyExpandTemplatesResponse :: ExpandTemplatesResponse
emptyExpandTemplatesResponse = ExpandTemplatesResponse
    { etExpandedText  = ""
    , etExpandedXml   = Nothing
    }

