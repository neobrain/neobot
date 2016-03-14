--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.Search
-- Description : Representing 'search' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'search' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.Search where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data SearchRequest
 = SearchRequest
    { srSearch     :: String
    , srNamespaces :: [NamespaceID]
    , srWhat       :: Bool -- True => (full)text search, o/w title search.
    , srRedirects  :: Bool
    , srOffset     :: Maybe String
    , srLimit      :: Maybe Int
    }

instance APIRequest SearchRequest where
  queryKind _ = QList "search"
  showReq r
   = [ mbOpt "srsearch" id (Just (srSearch r))
     , opt1  "srnamespace" (srNamespaces r)
     , opt "srwhat" ((\x -> if x then "text" else "title") (srWhat r))
     , optB  "srredirects" (srRedirects r)
     , mbOpt "sroffset" id (srOffset r)
     , mbOpt "srlimit"  show (srLimit r)
     ]

emptySearchRequest :: String -> SearchRequest
emptySearchRequest s = SearchRequest
    { srSearch     = s
    , srNamespaces = []
    , srWhat       = False
    , srRedirects  = False
    , srOffset     = Nothing
    , srLimit      = Nothing
    }
    
data SearchResponse
 = SearchResponse
    { srPages    :: [PageTitle]
    , srContinue :: Maybe String
    }

emptySearchResponse :: SearchResponse
emptySearchResponse = SearchResponse
    { srPages    = []
    , srContinue = Nothing
    }
    
