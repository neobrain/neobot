--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.WatchList
-- Description : Representing 'watchlist' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'watchlist' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.WatchList where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data WatchListRequest
 = WatchListRequest
    { wlAllRev     :: Bool
    , wlStart      :: Maybe Timestamp
    , wlEnd        :: Maybe Timestamp
    , wlNamespaces :: [NamespaceID]
    , wlDir        :: Maybe TimeArrow
    , wlLimit      :: Maybe Int
    , wlProp       :: [String]
    , wlShow       :: [String]
    }

instance APIRequest WatchListRequest where
  queryKind _ = QList "watchlist"
  showReq r
   = [ optB "wlallrev" (wlAllRev r)
     , mbOpt "wlstart" id (wlStart r)
     , mbOpt "wlend" id   (wlEnd r)
     , opt1  "wlnamespace" (wlNamespaces r)
     , mbOpt "wldir" (\ x -> if x==Earlier then "older" else "newer")
                     (wlDir r)
     , mbOpt "wllimit" show (wlLimit r)
     , opt1  "wlprop"  (wlProp r)
     , opt1  "wlshow"  (wlShow r)
     ]

emptyWatchListRequest :: WatchListRequest
emptyWatchListRequest = WatchListRequest
    { wlAllRev     = False
    , wlStart      = Nothing
    , wlEnd        = Nothing
    , wlNamespaces = []
    , wlDir        = Nothing
    , wlLimit      = Nothing
    , wlProp       = []
    , wlShow       = []
    }

data WatchListResponse 
 = WatchListResponse
    { wlWatch    :: WatchList
    , wlContinue :: Maybe String
    }

emptyWatchListResponse :: WatchListResponse
emptyWatchListResponse = WatchListResponse
   { wlWatch = emptyWatchList
   , wlContinue = Nothing
   }

data WatchList
 = WatchList
    { wlPage   :: PageTitle
    , wlRevId  :: Maybe RevID
    , wlUser   :: Maybe UserName
    , wlIsAnon :: Bool
    , wlIsNew  :: Bool
    , wlIsMinor :: Bool
    , wlIsPatrolled :: Bool
    , wlTimestamp :: Maybe Timestamp
    , wlLength  :: Maybe Int
    , wlOldLength :: Maybe Int
    , wlComment :: Maybe String
    }
    
emptyWatchList :: WatchList
emptyWatchList
 = WatchList
    { wlPage   = emptyPageTitle
    , wlRevId  = Nothing
    , wlUser   = Nothing
    , wlIsAnon = False
    , wlIsNew  = False
    , wlIsMinor = False
    , wlIsPatrolled = False
    , wlTimestamp = Nothing
    , wlLength    = Nothing
    , wlOldLength = Nothing
    , wlComment   = Nothing
    }
    
