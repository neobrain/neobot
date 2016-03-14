--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.OpenSearch
-- Description : Representing OpenSearch requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing OpenSearch requests.
--
--------------------------------------------------------------------
module MediaWiki.API.Action.OpenSearch where

data OpenSearchRequest
 = OpenSearchRequest
    { osSearch     :: String
    , osLimit      :: Maybe Int
    , osNamespaces :: Maybe [Int]
    }

emptyOpenSearchRequest :: String -> OpenSearchRequest
emptyOpenSearchRequest tit
 = OpenSearchRequest
    { osSearch     = tit
    , osLimit      = Nothing
    , osNamespaces = Nothing
    }

data OpenSearchResponse
 = OpenSearchResponse
      { osHits :: [OpenSearchHit]
      }

data OpenSearchHit
 = OpenSearchHit
      { oshTitle   :: String
      , oshMatches :: [String]
      }

