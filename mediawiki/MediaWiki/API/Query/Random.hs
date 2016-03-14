--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.Random
-- Description : Representing 'random' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'random' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.Random where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data RandomPagesRequest
 = RandomPagesRequest
    { rnNamespaces :: [NamespaceID]
    , rnLimit      :: Maybe Int
    }

instance APIRequest RandomPagesRequest where
  queryKind _ = QList "random"
  showReq r = [ opt1  "rnnamespace" (rnNamespaces r)
              , mbOpt "rnlimit" show (rnLimit r)
	      ]

emptyRandomPagesRequest :: RandomPagesRequest
emptyRandomPagesRequest = RandomPagesRequest
    { rnNamespaces = []
    , rnLimit      = Nothing
    }

data RandomPagesResponse
 = RandomPagesResponse
    { rnPages :: [PageTitle]
    }
    
emptyRandomPagesResponse :: RandomPagesResponse
emptyRandomPagesResponse
 = RandomPagesResponse
    { rnPages = []
    }
