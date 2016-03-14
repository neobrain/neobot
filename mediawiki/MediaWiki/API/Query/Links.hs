--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.Links
-- Description : Representing 'links' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'links' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.Links where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data LinksRequest
 = LinksRequest
    { plNamespaces   :: [NamespaceID]
    , plLimit        :: Maybe Int
    , plContinueFrom :: Maybe String
    }

instance APIRequest LinksRequest where
  queryKind _ = QProp "links"
  showReq r
   = [ opt1 "plnamespace" (plNamespaces r)
     , mbOpt "pllimit" show (plLimit r)
     , mbOpt "plcontinue" id (plContinueFrom r)
     ]


emptyLinksRequest :: LinksRequest
emptyLinksRequest 
 = LinksRequest
    { plNamespaces   = []
    , plLimit        = Nothing
    , plContinueFrom = Nothing
    }

data LinksResponse
 = LinksResponse
    { plPages    :: [(PageTitle,[PageTitle])]
    , plContinue :: Maybe String
    }

emptyLinksResponse :: LinksResponse
emptyLinksResponse = LinksResponse
    { plPages = []
    , plContinue = Nothing
    }
    
