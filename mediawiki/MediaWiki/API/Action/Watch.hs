--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.Watch
-- Description : Representing Watch requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing Watch requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Action.Watch where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data WatchRequest
 = WatchRequest
    { waTitle  :: PageName
    , waIsUnwatch :: Bool
    }

instance APIRequest WatchRequest where
  isPostable _ = True
  showReq r = 
   [ opt "title" (waTitle r)
   , optB "unwatch" (waIsUnwatch r)
   ]

emptyWatchRequest :: WatchRequest
emptyWatchRequest = WatchRequest
    { waTitle = ""
    , waIsUnwatch = False
    }
