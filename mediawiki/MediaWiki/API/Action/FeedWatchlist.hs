--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.FeedWatchlist
-- Description : Representing FeedWatchList requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing FeedWatchList requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Action.FeedWatchlist where

import MediaWiki.API.Types

data FeedWatchListRequest
 = FeedWatchListRequest
    { feAsAtom    :: Bool  -- False => rss
    , feHours     :: Maybe Int
    , feAllRev    :: Bool
    }

emptyFeedWatchListRequest :: FeedWatchListRequest
emptyFeedWatchListRequest = FeedWatchListRequest
    { feAsAtom    = False
    , feHours     = Nothing
    , feAllRev    = False
    }

data FeedWatchListResponse
 = FeedWatchListResponse
    { fwFeedFormat  :: String
    , fwFeedRaw     :: String
    , fwFeedItems   :: [FeedItem]
    }

data FeedItem
 = FeedItem
    { fiTitle     :: String
    , fiUrl       :: URLString
    , fiComment   :: String
    , fiTimestamp :: String
    , fiUser      :: String
    , fiText      :: String
    }

