--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.RecentChanges
-- Description : Representing 'recentchanges' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'recentchanges' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.RecentChanges where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data RecentChangesRequest
 = RecentChangesRequest
    { rcStart      :: Maybe Timestamp
    , rcEnd        :: Maybe Timestamp
    , rcDir        :: Maybe TimeArrow
    , rcNamespaces :: [NamespaceID]
    , rcTitles     :: [PageName]
    , rcProp       :: [String]
    , rcShow       :: [String]
    , rcLimit      :: Maybe Int
    , rcType       :: [String]  -- edit,new,log
    }

instance APIRequest RecentChangesRequest where
  queryKind _ = QList "recentchanges"
  showReq r =
    [ mbOpt "rcstart" id (rcStart r)
    , mbOpt "rcend"   id (rcEnd r)
    , mbOpt "rcdir" (\ x -> if x == Earlier then "older" else "newer") (rcDir r)
    , opt1  "rcnamespace" (rcNamespaces r)
    , opt1  "rctitles" (rcTitles r)
    , opt1  "rcprop"   (rcProp r)
    , opt1  "rcshow"   (rcShow r)
    , mbOpt "rclimit" show (rcLimit r)
    , opt1  "rctype" (rcType r)
    ]

emptyRecentChangesRequest :: RecentChangesRequest
emptyRecentChangesRequest = RecentChangesRequest
    { rcStart      = Nothing
    , rcEnd        = Nothing
    , rcDir        = Nothing
    , rcNamespaces = []
    , rcTitles     = []
    , rcProp       = []
    , rcShow       = []
    , rcLimit      = Nothing
    , rcType       = []
    }


data RecentChangesResponse
 = RecentChangesResponse
    { rchChanges :: [RecentChange]
    , rchContinue :: Maybe String
    }
    
emptyRecentChangesResponse :: RecentChangesResponse
emptyRecentChangesResponse = RecentChangesResponse
    { rchChanges  = []
    , rchContinue = Nothing
    }

data RecentChange
 = RecentChange
    { rchType      :: Maybe String
    , rchPage      :: PageTitle
    , rchPageTo    :: Maybe PageTitle
    , rchRcId      :: Maybe RevID
    , rchRevId     :: Maybe RevID
    , rchRevOldId  :: Maybe RevID
    , rchUser      :: Maybe UserName
    , rchIsAnon    :: Bool
    , rchIsBot     :: Bool
    , rchIsNew     :: Bool
    , rchIsMinor   :: Bool
    , rchLength    :: Maybe Int
    , rchLengthOld :: Maybe Int
    , rchTimestamp :: Maybe String
    , rchComment   :: Maybe String
    }
    
emptyRecentChange :: RecentChange
emptyRecentChange
 = RecentChange
    { rchType      = Nothing
    , rchPage      = emptyPageTitle
    , rchPageTo    = Nothing
    , rchRcId      = Nothing
    , rchRevId     = Nothing
    , rchRevOldId  = Nothing
    , rchUser      = Nothing
    , rchIsAnon    = False
    , rchIsBot     = False
    , rchIsNew     = False
    , rchIsMinor   = False
    , rchLength    = Nothing
    , rchLengthOld = Nothing
    , rchTimestamp = Nothing
    , rchComment   = Nothing
    }
