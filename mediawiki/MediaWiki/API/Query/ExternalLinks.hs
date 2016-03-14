--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.ExternalLinks
-- Description : Representing 'extlinks' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'extlinks' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.ExternalLinks where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data ExternalLinksRequest
 = ExternalLinksRequest
     { elLimit  :: Maybe Int
     , elOffset :: Maybe String
     }
     
instance APIRequest ExternalLinksRequest where
  queryKind _ = QProp "extlinks"
  showReq r =
    [ mbOpt "ellimit" show (elLimit r)
    , mbOpt "eloffset" id (elOffset r)
    ]

emptyExternalLinksRequest :: ExternalLinksRequest
emptyExternalLinksRequest = ExternalLinksRequest
     { elLimit  = Nothing
     , elOffset = Nothing
     }
     
data ExternalLinksResponse
 = ExternalLinksResponse
     { elPages    :: [(PageTitle,[URLString])]
     , elContinue :: Maybe String
     }
     
emptyExternalLinksResponse :: ExternalLinksResponse
emptyExternalLinksResponse = ExternalLinksResponse
     { elPages    = []
     , elContinue = Nothing
     }
     
